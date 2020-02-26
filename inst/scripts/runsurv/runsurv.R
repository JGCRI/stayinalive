library('stayinalive')
library('hector')
library('foreach')
library('survival')

## Entry point for batch jobs
## taskid - array task id.  Should start at 0
## gcblksize - Number of grid cells in each block.  This was set in the previous
##             processing step.
## subblksize - Number of grid cells in each sub-block.  Sub-blocks allow us to process
##             a portion of the data in an input file in order to keep memory usage under
##             control
## inputdir - directory containing the numpy files
## outputdir - directory to write output into
run_sa <- function(taskid, gcblksize, subblksize, inputdir, outputdir, thresh=12, DEBUG = FALSE)
{
    ## Check to see that the outputdir exists and is writeable before we do all this work
    outfilename <- file.path(outputdir, sprintf("survival-analysis_%03d.rds", taskid))
    assertthat::is.writeable(outputdir)

    doParallel::registerDoParallel(cores=24)

    ## iterate over the 4 rcps, collect the events for a group of grid cells
    ## for all of the runs, and perform the survival analysis

    message('Subblock size= ', subblksize)
    rcps_list <- lapply(1:4,
                        function(ircp) {
                            message('Creating events for ircp= ', ircp)
                            rcpevents(taskid, gcblksize, subblksize, ircp, inputdir, thresh)
                        })

    if(DEBUG) {
        dbgfile <- 'debug.rds'
        saveRDS(rcps_list, dbgfile)
    }

    ## This is redundant with similar code in rcpevents(), but oh, well.  We need these
    ## constants to turn our cell indices into global cell indices
    ncell <- 67420                      # Total number of land grid cells at half-degree resolution
    nblock <- ceiling(ncell / gcblksize)
    nsubblk <- ceiling(gcblksize / subblksize)
    blk <- taskid %% nblock
    subblk <- floor(taskid / nblock)

    cellid_offset <- blk*gcblksize + subblk*subblksize

    ### Now, for each grid cell, concatenate the event tables from all 4 rcps and run the
    ### proportional hazard analysis.  For each grid cell we will get a (single-row) table of:
    ### cell_id  coef  expcoef  secoef  z  pvalue
    message('Running coxph.')
    nrcp <- length(rcps_list)
    survival_list <-
        foreach(cellid = seq_along(rcps_list[[1]])) %dopar% {
                events <- data.table::rbindlist(
                lapply(seq(1,nrcp),
                       function(i) {
                           rcps_list[[i]][[cellid]]
                       }))
            sa <- summary(coxph(Surv(tstart, tstop, drought)~Tg, data=events))
            df <- as.data.frame(sa$coefficients)
            ## The names of the covariates are stored in the row names.  We could turn these
            ## into another column, but since we only have one covariate (Tg), we can dispense
            ## with the names entirely
            row.names(df) <- NULL
            df$cellid <- cellid + cellid_offset
        }

    ## Now we need to collect this into a single table and write it out.  We could have
    ## done this with the .combine feature of foreach, but I don't trust it to bind rows
    ## efficiently.
    survival_tbl <- data.table::rbindlist(survival_list)

    ## Now we just need to write it out.
    saveRDS(survival_tbl, outfilename, compress=TRUE)
}

## taskid - array task id.  Should start at 0
## gcblksize - Number of grid cells in each block.  This was set in the previous
##             processing step.
## subblksize - Number of grid cells in each sub-block.  Sub-blocks allow us to process
##             a portion of the data in an input file in order to keep memory usage under
##             control
## inputdir - directory containing the numpy files
rcpevents <- function(taskid, gcblksize, subblksize, ircp,
                      inputdir, thresh=12)
{
    ncell <- 67420                      # Total number of land grid cells at half-degree resolution
    nblock <- ceiling(ncell / gcblksize)
    nsubblk <- ceiling(gcblksize / subblksize)

    ## The task id is calculated as:
    ## tid = blk + Nblk*subblk            (all counts are zero-indexed).
    ## This arrangement makes it more likely that concurrent processes (which are likely to
    ## have adjacent task ids) will be looking at different files.
    ## Therefore:  b = tid mod Nblk
    ##             s = floor(tid / Nblk)
    blk <- taskid %% nblock
    subblk <- floor(taskid / nblock)

    rcps <- c('rcp26', 'rcp45', 'rcp60', 'rcp85')
    rcp <- rcps[ircp]
    blkpattern <- sprintf('%s_batch-%03d\\.pkl', rcp, blk)

    pklfiles <- list.files(inputdir, blkpattern, full.names=TRUE)
    message('pklfiles:\n', paste("\t",pklfiles, collapse='\n'))
    stopifnot(length(pklfiles) == 4)   # 4 models

    strt <- subblksize * subblk + 1
    end <- (subblk + 1) * subblksize          # no +1 b/c the end point is included

    cellrng <- seq(strt,end)

    celldata <- get_subblk(pklfiles, cellrng)   # returns a list of matrices, one for each grid cell,
                                                # containing all runs (all ESMs and realizations) for each cell

    years <- 1861:2099

    ## We need global mean temperatures.  These came from hector runs in the
    ## simulation, so that's where we will get them from here.
    inifile_base <- paste0('hector_',rcp,'.ini')
    inifile <- system.file('input',inifile_base, package='hector')
    hcore <- newcore(inifile)
    run(hcore, 2100)
    tgav <- fetchvars(hcore, years, GLOBAL_TEMP())[['value']]


    scenarioid <- ircp                         # used to compute unique ids across RCP groupings

    ## Return the list of event tables for each grid cell
    foreach(i=seq_along(celldata)) %do% {
        ## apply the duration threshold
        #message('....applying duration threshold')
        ## get the time series for all the runs of the ith grid cell
        tsmat <- celldata[[i]]
        ## Apply the duration threshold.  apply binds the modified time series by column
        ## instead of row, so we have to transpose the result to get time in columns again.
        tsmat <- t(apply(tsmat, 1, function(x) {apply_duration_thresh(x, thresh)}))

        #message('....converting to events')
        ## Event conversion:  the groupings are runs.  There are
        nrun <- nrow(tsmat)               # Number of runs in the
        events_list <- tsmat2event(tsmat, tgav, scenarioid, nrun, length(years))
    }
}
