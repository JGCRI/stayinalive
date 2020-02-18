.onLoad <- function(libname, pkgname)
{
    py3 <- system2('which', 'python3', stdout=TRUE)
    reticulate::use_python(py3)
}
