#' Read an array or matrix from a numpy save file
#'
#' @param filename Name of the file to read
#' @return An R array read from the data in the file.
#' @export
readNPY <- function(filename)
{
    np <- reticulate::import('numpy')
    np$load(filename)
}

#' Read a pickled python object from a file
#'
#' Read data saved with the python \code{pickle} module.
#'
#' Python lists will return come through as lists; numpy arrays will come through as R
#' arrays (or matrices in 2-d).  I haven't tested what dictionaries turn into, but I'd
#' guess something like a named list.  If you try it sometime, let me know.
#'
#' @param filename Name of the file to read
#' @param sublist If non-\code{NULL}, return the only the indices given (this will fail
#' if the object read from the file is not a list).
#' @return An R object that more or less works like the python object; see details.
#' @export
readpkl <- function(filename, sublist=NULL)
{
    bi <- reticulate::import('builtins')
    pkl <- reticulate::import('pickle')
    fh <- bi$open(filename,'rb')
    rtn <- pkl$load(fh)
    fh$close()
    if (is.null(sublist)) {
        rtn
    }
    else {
        stopifnot(is.list(rtn))
        inrange <- sublist >= 1 & sublist <= length(rtn)
        rtn[sublist[inrange]]
    }
}

#' Read a specific group of cells from a list of python pickle files
#'
#' Given a list of python pickle files, each containing runs for the same block of
#' grid cells, extract the same subblock from each input file and merge them into a
#' single dataset with all of the runs for each grid cell.
#'
#' The input files should be python pickle files, each containing a list of matrices.
#' Each matrix should contain runs for a single grid cell, with runs in rows and time
#' steps in columns.  All of the input files should contain data for the same grid cells,
#' in the same order.
#'
#' @param filelist List of input filenames
#' @param cellrng Vector of cell indices to extract
#' @return A list of matrices.  Each matrix contains the merged set of runs from all of
#' the input files for a single grid cell.
#' @export
get_subblk <- function(filelist, cellrng)
{
    message(date(), ':  Reading data...')
    rawdata <- lapply(filelist, readpkl, sublist=cellrng)
    message('\t... done.  Rawdata size= ')
    print(object.size(rawdata), units='auto')
    ## Now have a list of list of matrices. We need to convert this into a list of
    ## matrices.
    message(date(), ':  Combining matrices...')
    rslt <-
        lapply(seq_along(rawdata[[1]]),
               function(j) {
                   mlist <- lapply(seq_along(rawdata), function(i) {rawdata[[i]][[j]]})
                   do.call(rbind, mlist)
               })
    message('\t... done.  Result size= ')
    print(object.size(rslt), units='auto')
    rslt
}
