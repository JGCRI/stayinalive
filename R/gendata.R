#### Functions for generating synthetic data sets for testing survival analysis

#' Generate a matrix of drought time series for a selection of basins.
#'
#'
#' @param nbasin Number of basins
#' @param nmonth Number of months
#' @param Tg Temperature
#' @param expalpha Baseline probability per month of a drought beginning
#' @param beta Temperature coefficient for relative risk
#' @param L0 Minimum drought length
#' @param L1 Maximum drought length
#' @importFrom assertthat assert_that
#' @export
gents <- function(nbasin, nmonth, Tg = rep(0, nmonth), expalpha = 1.0/300.0, beta = 0.1,
                          L0 = 12, L1 = 24)
{
    assert_that(length(Tg) == nmonth)

    alpha <- log(expalpha)

    output <- matrix(nrow=nbasin, ncol=nmonth)
    status <- rep(0, nbasin)            # Months left in the current drought
    prob <- exp(alpha + beta * Tg)      # Drought probability by time

    for(m in 1:nmonth) {
        ## Decrement the lengths of the droughts in progress
        status <- ifelse(status>0, status-1, 0)
        r <- runif(nbasin)              # roll for drought
        d <- runif(nbasin, L0, L1)      # roll for length

        ## In basins where a drought is not in progress, start a new one of the
        ## indicated length
        status <- ifelse(status == 0 & r < prob[m], d, status)

        output[ , m] <- ifelse(status > 0, 1, 0) # Record the drought status in
                                        # each basin
    }

    output
}



#' Convert a single-basin drought time series into a data frame of event data
#'
#' The vector of status values will be converted into a data frame that
#' describes the waiting times for drought events and the changes in their
#' time-varying covariates.  Currently, the only covariate we are tracking is
#' Global mean temperature. Changes in Tg will be recorded at annual
#' boundaries.
#'
#' An event data frame consists of the following columns:
#' \describe{
#'   \item{id}{A unique serial number for each drought event.}
#'   \item{basinid}{A unique id for each basin.  This id will be used as a
#' grouping variable in frailty models.}
#'   \item{tstart}{Start time for a recording period, in months since the last event (or
#' start of the scenario).  Covariate changes take effect at the start of a
#' period.}
#'   \item{tstop}{End time for a recording period, in months since the last
#' event.  Events during a period are deemed to occur at the end of the period.}
#'   \item{Tg}{Temperature covariate}
#'   \item{drought}{Indicator of whether the period had a drought.}
#' }
#'
#' This processing prepares the data in a time series of drought/no drought
#' indicators to be used in a survival analysis.  Each basin can have multiple
#' events, and these will be treated as separate subjects.  The reason we do
#' this is that it's the easiest way to account for the fact that droughts have
#' duration, and while a drought is in progress you can't have another drought.
#' This turns out to be rather difficult to do in a counting formulation.
#'
#' The intent is to use this data in a proportional hazard model, so by default
#' right censored events are not included.  Likewise, we make no effort to
#' adjust for the fact that for the first drought in a time series, we don't
#' actually know the full time since the last drought.
#'
#' @param ts Vector of monthly drought status.  0= no drought; >0= drought
#' @param Tg Vector of \emph{annual} global mean temperature.
#' @param basinid Numerical identifier for the basin.  This \emph{must} be
#' unique for each basin.
#' @param scenarioid Numerical identifier for the scenario.  This \emph{must} be
#' unique across scenarios.
#' @param nbasin Total number of basins.  This is used in constructing unique id
#' values from the scenarioid and basinid.
#' @param neventmax Maximum number of events for a single basin and scenario.
#' This is also used to construct UIDs.
#' @param censored Flag indicating whether we should account for left
#' and right censoring (see details).
#' @return Data frame of event data.
#' @importFrom assertthat assert_that
#' @export
ts2event <- function(ts, Tg, basinid, scenarioid, nbasin=300, neventmax=100,
                     censored=FALSE)
{
    assert_that(censored==FALSE,
                msg='Adjustment for censoring not yet implemented')

    ts[ts>0] <- 1
    rr <- rle(ts)
    if(!any(rr$values==1)) {
        ## This time series did not have any droughts.  Return an empty data
        ## frame that will be compatible for rbinding with data frames that have
        ## content.
        return(data.frame(id=integer(0), basinid=integer(0), tstart=numeric(0),
                          tstop=numeric(0), Tg=numeric(0), status=integer(0)))
    }

    ## Find the starting indices of each drought
    index <- 1 + c(0, cumsum(rr$lengths)) # start locations of runs of 0/1
    events <- which(rr$values==1)
    if(events[1] == 1) {
        ## If there is an event in progress in the first time step, drop it
        ## because we don't know when it really started, nor what the covariates
        ## were at that time.
        ## TODO: adjust for left truncation in this case?
        events <- events[-1]
    }
    event_starts <- index[events] # index (month number) of the start of each event
    prev_event_ends <- index[events-1]
    event_wait_times <- event_starts - prev_event_ends


    nevent <- length(event_starts)
    if(nevent > neventmax) {
        warning('Number of events greater than eventmax (events=', nevent,
                ' max=',neventmax,') Only the first ',neventmax,
                ' events will be recorded.')
        nevent <- neventmax
    }

    wtimes <-
        lapply(1:nevent,
               function(eventid) {
                   uid <- ((scenarioid-1)*nbasin + (basinid-1)) * neventmax + eventid

                   startmonth <- event_starts[eventid]   # absolute start of the drought
                   dtime <- event_wait_times[eventid]    # waiting time since the end of the last drought

                   drought_data <-
                       data.frame(id=uid, basinid=basinid, dtime=dtime)
                   event_data <- tmerge(drought_data[1:2], drought_data, id=id, drought=event(dtime))

                   ## Add the change in covariate events.  Global temperature is observed at
                   ## the beginning of each year.
                   waitstrt <- startmonth - dtime                 # start of the waiting period prior to this drought
                   wstrtyr <- as.integer(floor((waitstrt-1)/12)+1)  # year in which the waiting period started
                   wstrttemp <- Tg[wstrtyr]
                   wendyr <- as.integer(floor((startmonth-1)/12)+1)  # year in which the waiting period ended
                   ## Do we need to do something special if the drought started in the first month of a year?
                   if(wendyr > wstrtyr) {
                       yr <- seq(wstrtyr+1, wendyr)                # Absolute years in the waiting period
                       waityr <- yr-wstrtyr                      # Year count in the waiting period
                       tstart <- 12*waityr + 1 - waitstrt        # Month count in the waiting period, at yearly intervals
                       temp <- Tg[yr]                            # Global mean temperature for each year
                       gmtemp <- data.frame(id=uid, basinid=basinid, time=tstart, temp=temp)
                   }
                   else {
                       gmtemp <- data.frame(id=uid, basinid=basinid, time=0, temp=wstrttemp)
                   }
                   tmerge(event_data, gmtemp, id=id, Tg=tdc(time, temp), options=list(tdcstart=wstrttemp))
               })

    do.call(rbind, wtimes)
}


#' @describeIn ts2event Convert a matrix of basin time series to an event data frame
#'
#' @param tsmat A matrix of basin time series. Each row of the matrix should be
#' a time series for one basin.
#' @importFrom foreach foreach %do% %dopar%
#' @export
tsmat2event <- function(ts, Tg, scenarioid, nbasin=300, neventmax=100,
                        censored=FALSE)
{
    foreach(basinid=seq(1, nrow(ts)), .combine=rbind) %dopar% {
        ts2event(ts[basinid,], Tg, basinid, scenarioid, nbasin, neventmax, censored)
    }
}
