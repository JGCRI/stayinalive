
#' Pull temperature from a single grid cell in a group of netCDF files
#'
#' Retrieve the temperature for a single grid cell specified by latitude and
#' longitude.
#'
#' The varnames list allows you to override the default variable names.  You
#' only have to specify the ones that are changed from their defaults.  The
#' names recognized are:  latvar, lonvar, timevar, varname.
#'
#' @param filelist Names of netCDF files to read.
#' @param lat Latitude of pixel to pull.
#' @param lon Longitude of pixel to pull.
#' @param varnames Named list of variable names
#' @return Data frame with time, global temperature, and grid cell temperature.
#' @export
get_ncdf_pixel <- function(filelist, lat=39.7, lon=-78.3, varnames=NULL)
{
    dplyr::bind_rows(lapply(filelist, get_ncdf_pixel_1, latget=lat, longet=lon, varnames=varnames))
}


## Helper function for get_ncdf_pixel
get_ncdf_pixel_1 <- function(file, latget, longet, varnames)
{
    latvar <- 'lat'
    lonvar <- 'lon'
    timevar <- 'time'
    varname <- 'tas'
    for (v in names(varnames)) {
        assign(v, varnames[[v]])
    }

    ncfil <- ncdf4::nc_open(file)
    var3d <- ncdf4::ncvar_get(ncfil, var=varname)
    lat <- ncdf4::ncvar_get(ncfil, var=latvar)
    lon <- ncdf4::ncvar_get(ncfil, var=lonvar)
    time <- ncdf4::ncvar_get(ncfil, var=timevar)

    nlat <- length(lat)
    nlon <- length(lon)
    ntime <- length(time)

    assertthat::assert_that(all(dim(var3d) == c(nlon, nlat, ntime)))

    ## Reorganize the data to put latitude in the first slot.  This will make it
    ## easier to get the area-weighted variable values.
    var3d <- aperm(var3d, c(2,1,3))
    areafac <- as.vector(cos(lat*pi/180.0))
    areafac <- areafac / (sum(areafac)*nlon)
    varavg <- apply(var3d, 3, function(grid) {sum(areafac*grid)})

    ## Now we need to figure out which grid cell we need to grab.  Take
    ## whichever one is closest.
    if(longet < 0 && min(lon)>=0) {
        ## lon is given as 0-360 instead of -180-+180
        longet <- longet + 360.0
    }
    i <- which.min(abs(lat - latget))
    j <- which.min(abs(lon - longet))

    var <- var3d[i,j, ]

    t <- (time-181.25)/365 + 2006
    data.frame(year=t, varavg=varavg, var=var)
}

#' Format data for the plots that we want to make
#'
#' Input to this function is the data frame from \code{\link{get_ncdf_pixel}}.
#'
#' @param df Input data frame
#' @return Data frame with colnames adjusted and extra columns added.
#' @importFrom dplyr %>%
#' @export
reformat_data <- function(df)
{
    df <- dplyr::rename(df, Tg=varavg, Temperature=var)

    ## Subtract off the minimum temperature, so we get temperature change rather
    ## than absolute temp.  For the grid cell temperature we will just convert
    ## to Celsius (this works because any reasonable grid cell will have an
    ## annual average temperature above 0C, but if you pick a polar cell,
    ## beware!)
    Tg0 <- min(df$Tg)
    df <- dplyr::mutate(df, Tg=Tg-Tg0, Temperature=Temperature-273.15) %>%
      dplyr::mutate(tgbin=cut(Tg, breaks=25))

    ## Rename each global temperature bin with its
    binavg <- dplyr::group_by(df, tgbin) %>% dplyr::summarise(binavg=mean(Tg))
    df <- dplyr::left_join(df, binavg, by='tgbin')
    df <- dplyr::mutate(df, tgbin=factor(round(100*binavg)/100), cens=1) %>% dplyr::select(-binavg)

    df
}
