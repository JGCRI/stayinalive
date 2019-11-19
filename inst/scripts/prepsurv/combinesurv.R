## For each grid cell, gather up all of the temporary outputs and combine them
## into a single dataset
## taskid - array task id.  Should start at 0
## ntask - number of tasks in the array
## outputdir - root directory for output
gather_temp_files <- function(taskid, ntask, outputdir)
{
    ngrid <- 67420
    nbatch <- ceiling(ngrid / ntask)
    strt <- taskid*nbatch + 1
    end <- (taskid + 1)*nbatch
    if(end > ngrid) {
        end <- ngrid
    }

    doParallel::registerDoParallel(cores=24)
    foreach(cellid = seq(strt,end)) %dopar% {
        batchid <- floor(cellid/100)
        indir <- file.path(outputdir, batchid, cellid)
        tempfiles <- list.files(indir, pattern='temp_drgt_event.*\\.rds', full.names=TRUE)
        events <- dplyr::bind_rows(lapply(tempfiles, readRDS))
        outfile <- file.path(indir, paste0('drought-events-',cellid,'.rds'))
        saveRDS(events, outfile)
    }
}
