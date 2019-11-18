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
#' @return Data frame of event data.
#' @importFrom assertthat assert_that
#' @export
ts2event <- function(ts, Tg, basinid, scenarioid, nbasin=300, neventmax=100)
{
    ## Start months for each year
    yearstarts <- (seq_along(Tg)-1)*12 + 1

    ## Sometimes the drought events are tagged with their duration, but we don't
    ## really need that, just drought, no drought indicator.
    ts[ts>0] <- 1
    rr <- rle(ts)

    ## Although we talk about droughts as "events", what we really want from
    ## this time series is the waiting times
    waitidx <- which(rr$values == 0)
    nwait <- length(waitidx)
    if(nwait == 0) {
        ## Drought for the entire run.  It's rare, but with 67420 grid cells and a lot
        ## of scenarios it happens occasionally.
        return(NULL)
    }
    ## Each wait ended with a drought, except possibly the last, which could
    ## have ended with the end of the time series
    waitstatus <- rep(1, nwait)
    if(waitidx[nwait] == length(rr$values)) {
        ## This observation was censored
        waitstatus[nwait] <- 0
    }

    ## Find the starting and ending indices of each wait, along with the length
    index <- cumsum(c(1, rr$lengths))   # start locations of runs of 0/1, adjust
                                        # for unit indexing
    waitstrt <- index[waitidx]
    waitend  <- index[waitidx+1]
    waittimes <- rr$lengths[waitidx]

    nevent <- length(waitstrt)
    if(nevent > neventmax) {
        ## This is solely about making sure we can assign unique identifiers
        warning('Number of events greater than eventmax (events=', nevent,
                ' max=',neventmax,') Only the first ',neventmax,
                ' events will be recorded.')
        nevent <- neventmax
    }

    wtimes <-
        lapply(1:nevent,
               function(eventid) {
                   uid <- ((scenarioid-1)*nbasin + (basinid-1)) * neventmax + eventid

                   startmonth <- waitstrt[eventid]       # start of the waiting period
                   endmonth <- waitend[eventid]
                   wtime <- waittimes[eventid]
                   status <- waitstatus[eventid]

                   ## Gather these into a data frame to be used by tmerge.
                   drought_data <-
                       data.frame(id=uid, basinid=basinid, dtime=wtime, status=status)
                   event_data <- survival::tmerge(drought_data[1:2],
                                                  drought_data, id=id,
                                                  drought=event(dtime, status))

                   ## Add the change in covariate events.  Global temperature is observed at
                   ## the beginning of each year.
                   startyear <- as.integer(floor((startmonth-1)/12)+1)  # year
                                        # in which the waiting period started
                                        # (used for initial temperature.
                   starttemp <- Tg[startyear]

                   ## Find the year changes that occur during the waiting period
                   yearidx <- which(yearstarts > startmonth & yearstarts < endmonth)

                   if(length(yearidx) > 0) {
                       tstart <- yearstarts[yearidx] - startmonth
                       temp <- Tg[yearidx]                      # Global mean temperature for each year
                       gmtemp <- data.frame(id=uid, basinid=basinid, time=tstart, temp=temp)
                   }
                   else {
                       gmtemp <- data.frame(id=uid, basinid=basinid, time=0, temp=starttemp)
                   }
                   survival::tmerge(event_data, gmtemp, id=id, Tg=tdc(time, temp), options=list(tdcstart=starttemp))
               })

    do.call(rbind, wtimes)
}


#' @describeIn ts2event Convert a matrix of basin time series to an event data frame
#'
#' @param tsmat A matrix of basin time series. Each row of the matrix should be
#' a time series for one basin.
#' @importFrom foreach foreach %do% %dopar%
#' @export
tsmat2event <- function(tsmat, Tg, scenarioid, nbasin=300, neventmax=100)
{
    foreach(basinid=seq(1, nrow(tsmat)), .combine=rbind) %dopar% {
        ts2event(tsmat[basinid,], Tg, basinid, scenarioid, nbasin, neventmax)
    }
}
