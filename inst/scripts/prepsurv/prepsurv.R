library('stayinalive')
library('hector')

## Entry point for the batch scripts
## taskid - array task id.  Should start at 0
## ntask - number of tasks in the array
## inputdir - directory containing the numpy files
## outputdir - directory to write event files into
npy2event <- function(taskid, ntask, inputdir, outputdir, thresh=12)
{
    ## The input dir has a bunch of numpy files.
    npyfiles <- list.files(inputdir, '\\.npy', full.names=TRUE)
    print(npyfiles)
    nfile <- length(npyfiles)
    nbatch <- ceiling(nfile / ntask)
    strt <- taskid * nbatch + 1
    end <- (taskid + 1)*nbatch          # no +1 b/c the end point is included

    print(paste('strt: ', strt))
    print(paste('end: ', end))
    infiles <- npyfiles[strt:end]
    cat('infiles:\n')
    cat(infiles, collapse='\n')

    doParallel::registerDoParallel(cores=24)

    years <- 1861:2099

    for(i in seq_along(infiles)) {
        file <- infiles[i]
        scenarioid <- strt + i
        message('Processing file:  ', file)
        grid <- readNPY(file)

        ## apply the duration threshold
        message('....applying duration threshold')
        grid <- t(apply(grid, 1, function(x) {apply_duration_thresh(x, thresh)}))

        ## We need global mean temperatures.  These came from hector runs in the
        ## simulation, so that's where we will get them from here.
        ## Unfortunately, we have to do this every time because we have no way
        ## of knowing whether
        rcp <- stringr::str_match(file, '_(rcp[0-9]+)_')[1,2]
        inifile_base <- paste0('hector_',rcp,'.ini')
        inifile <- system.file('input',inifile_base, package='hector')
        hcore <- newcore(inifile)
        run(hcore, 2100)
        tgav <- fetchvars(hcore, years, GLOBAL_TEMP())[['value']]

        message('....converting to events')
        events <- tsmat2event(grid, tgav, scenarioid, 67420, length(years))

        message('....separating and writing')
        separate_and_write(events, scenarioid, outputdir)
        message('....done')
    }
}


## Separate event arrays by grid cell (called "group" in the output) and write
## them to the temporary files.
separate_and_write <- function(events, scenarioid, outputdir)
{
    events_splt <- split(events, events$groupid)
    foreach(ev = events_splt) %dopar% {
        cellid <- ev$groupid[1]
        batchid <- floor(cellid/100)
        outfile <- file.path(outputdir, batchid, cellid,
                             paste0('temp_drgt_event_', cellid, '_', scenarioid, '.rds'))
        saveRDS(ev, outfile)
    }
}

