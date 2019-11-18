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
