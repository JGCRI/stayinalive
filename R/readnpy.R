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
#' @return An R object that more or less works like the python object; see details.
#' @export
readpkl <- function(filename)
{
    bi <- reticulate::import('builtins')
    pkl <- reticulate::import('pickle')
    fh <- bi$open(filename,'rb')
    rtn <- pkl$load(fh)
    fh$close()
    rtn
}
