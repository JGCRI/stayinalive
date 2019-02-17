context('Converting data')

foreach::registerDoSEQ()

nyear <- 10
nmonth <- nyear*12
Tgyr <- seq(0.5, 2.5, length.out=nyear)
Tgmon <- rep(Tgyr, rep(12, nyear))

## Sample time series
ts1 <- rep(0, nmonth)
ts2 <- rep(0, nmonth); ts2[5:10] <- 1 # first year
ts3 <- rep(0, nmonth); ts3[15:24] <- 1 # second year
ts4 <- rep(0, nmonth); ts4[36:62] <- 1 # lasts several years

ts5 <- ts3 + ts4                     # two events
ts6 <- ts5; ts6[36:62] <- 0; ts6[25:27] <- 1; ts6[65:70] <- 1

test_that('No drought produces empty result', {
    ev <- ts2event(ts1, Tgyr, 1, 1)
    expect_equal(nrow(ev), 0)
})


test_that('One drought produces the expected result', {
    ev2 <- ts2event(ts2, Tgyr, 2, 1)
    expected2 <- data.frame(id=101, basinid=2, tstart=0, tstop=4, drought=1,
                            Tg=Tgyr[1])
    expect_equivalent(ev2, expected2)

    ev3 <- ts2event(ts3, Tgyr, 3, 1)
    expected3 <- data.frame(id=201, basinid=3, tstart=c(0,12), tstop=c(12,14),
                            drought=c(0,1), Tg=Tgyr[1:2])
    expect_equivalent(ev3, expected3)

    ev4 <- ts2event(ts4, Tgyr, 4, 1)
    expected4 <- data.frame(id=301, basinid=4, tstart=c(0,12,24),
                            tstop=c(12,24,35), drought=c(0,0,1), Tg=Tgyr[1:3])
    expect_equivalent(ev4, expected4)
})


test_that('Multiple droughts produce the expected result', {
    ev <- ts2event(ts5, Tgyr, 5, 1)
    expected <- data.frame(id=c(401,401,402), basinid=5, tstart=c(0,12,0),
                           tstop=c(12,14,11), drought=c(0,1,1), Tg=Tgyr[1:3])
    expect_equivalent(ev, expected)

    ev2 <- ts2event(ts6, Tgyr, 6, 1)
    expected2 <- data.frame(id=rep(c(501,502), c(2,4)), basinid=6,
                           tstart=c(0,12,0,9,21,33), tstop=c(12,14,9,21,33,37),
                           drought=c(0,1,0,0,0,1), Tg=c(Tgyr[1:2], Tgyr[3:6]))
    expect_equivalent(ev2, expected2)
})


test_that("Matrix of time series produces the same result as individual", {
    allts <- matrix(c(ts1, ts2, ts3, ts4, ts5, ts6), nrow=6, byrow=TRUE)
    allev <- tsmat2event(allts, Tgyr, 1)

    ## ts1 produces no events, so we omit it when constructing the comparison.
    ev2 <- ts2event(ts2, Tgyr, 2, 1)
    ev3 <- ts2event(ts3, Tgyr, 3, 1)
    ev4 <- ts2event(ts4, Tgyr, 4, 1)
    ev5 <- ts2event(ts5, Tgyr, 5, 1)
    ev6 <- ts2event(ts6, Tgyr, 6, 1)

    ev_expected <- rbind(ev2,ev3,ev4,ev5,ev6)

    expect_equivalent(allev, ev_expected)
})
