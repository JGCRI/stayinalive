

#' Make the ticker plot of drought incidence over time
#'
#' @param tsmat Matrix of time series: ensemble runs in rows, time (months) in
#' columns
#' @param thresh Duration threshold.  Droughts less than this duration will be
#' filtered.
#' @param yrstrt Start year.  Used to compute axis labels
#' @param col Color to use to plot the tickers
#' @param siz Size to plot the ticker dots.
#' @export
plot_ticker <- function(tsmat, thresh, yrstrt, col='darkgrey', siz=0.7)
{
    ## Filter to just droughts longer than the threshold.  A side effect of this is
    ## that the matrix will be transposed to have months in rows and ensemble runs in
    ## columns, but that's what we eventually want anyhow.
    tsmat <- apply(tsmat, 1, apply_duration_thresh, thresh=thresh)
    tsmat[tsmat > 0] <- 1               # convert duration into drought/no-drought flag
    nensemble <- ncol(tsmat)
    colnames(tsmat) <- seq(1, nensemble)
    tsdf <- as.data.frame(tsmat)
    tsdf$month <- seq(1,nrow(tsdf))

    ensdf <- tidyr::gather(tsdf, key='runid', value='value', -month, convert=TRUE)
    ensdf$drought <- ensdf$runid * ensdf$value
    ensdf$year <- (ensdf$month-1) / 12 + yrstrt
    ggplot2::ggplot(data=ensdf, ggplot2::aes(x=year, y=drought)) + ggplot2::geom_point(color=col) +
        ggplot2::ylim(c(1,nensemble+1)) + ggplot2::ylab('Ensemble run')
}
