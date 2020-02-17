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

test_that('No drought produces a single censored event', {
    ev <- ts2event(ts1, Tgyr, 1, 1)
    expected <- data.frame(id=1, groupid=1, tstart=seq(0, (nyear-1)*12, 12),
                           tstop=seq(12, nyear*12, 12), drought=0, Tg=Tgyr)
    ## use expect_equivalent since the survival package adds a bunch of attributes that
    ## are a black box to us.
    expect_equivalent(ev, expected)
    ## expect_equivalent doesn't check column names, since they are technically attributes,
    ## so check them explicitly.
    expect_setequal(names(ev), names(expected))
})


test_that('One drought produces the expected result', {
    ev2 <- ts2event(ts2, Tgyr, 2, 1)
    expected2 <- data.frame(id=c(101, rep(102, nyear)),
                            groupid=2,
                            tstart=c(0, 0, seq(2, 98, 12)),
                            tstop=c(4, 2, seq(14, 110, 12)),
                            drought=c(1, rep(0, nyear)),
                            Tg=c(Tgyr[1], Tgyr))
    expect_equivalent(ev2, expected2)
    expect_setequal(names(ev2), names(expected2))


    ev3 <- ts2event(ts3, Tgyr, 3, 1)
    expected3 <- data.frame(id=c(201, 201, rep(202, nyear-2)),
                            groupid=3,
                            tstart=c(0,12, seq(0, 84, 12)),
                            tstop=c(12,14, seq(12, 96, 12)),
                            drought=c(0,1, rep(0, nyear-2)),
                            Tg=c(Tgyr[1:2], Tgyr[-c(1,2)]))
    expect_equivalent(ev3, expected3)
    expect_setequal(names(ev3), names(expected3))


    ev4 <- ts2event(ts4, Tgyr, 4, 1)
    expected4 <- data.frame(id=c(rep(301,3), rep(302, 5)),
                            groupid=4,
                            tstart=c(0,12,24, 0,10,22,34,46),
                            tstop=c(12,24,35, seq(10,58,12)),
                            drought=c(0,0,1, rep(0,5)),
                            Tg=c(Tgyr[1:3], Tgyr[-(1:5)]))
    expect_equivalent(ev4, expected4)
    expect_setequal(names(ev4), names(expected4))
})


test_that('Multiple droughts produce the expected result', {
    ev <- ts2event(ts5, Tgyr, 5, 1)
    expected <- data.frame(id=c(401,401,402, rep(403, 5)),
                           groupid=5,
                           tstart=c(0,12,0, 0,10,22,34,46),
                           tstop=c(12,14,11, seq(10,58,12)),
                           drought=c(0,1,1, rep(0,5)),
                           Tg=c(Tgyr[1:3], Tgyr[-(1:5)]))
    expect_equivalent(ev, expected)
    expect_setequal(names(ev), names(expected))

    ev2 <- ts2event(ts6, Tgyr, 6, 1)
    expected2 <- data.frame(id=rep(c(501, 502, 503), c(2,4,5)),
                            groupid=6,
                            tstart=c(0,12, 0,9,21,33, 0,2,14,26,38),
                            tstop=c(12,14, 9,21,33,37, seq(2,50,12)),
                            drought=c(0,1,0,0,0,1, rep(0,5)),
                            Tg=c(Tgyr[1:2], Tgyr[3:6], Tgyr[-(1:5)]))
    expect_equivalent(ev2, expected2)
    expect_setequal(names(ev2), names(expected2))
})


test_that("Matrix of time series produces the same result as individual", {
    allts <- matrix(c(ts1, ts2, ts3, ts4, ts5, ts6), nrow=6, byrow=TRUE)
    allev <- tsmat2event(allts, Tgyr, 1)

    ev1 <- ts2event(ts1, Tgyr, 1, 1)
    ev2 <- ts2event(ts2, Tgyr, 2, 1)
    ev3 <- ts2event(ts3, Tgyr, 3, 1)
    ev4 <- ts2event(ts4, Tgyr, 4, 1)
    ev5 <- ts2event(ts5, Tgyr, 5, 1)
    ev6 <- ts2event(ts6, Tgyr, 6, 1)

    ev_expected <- rbind(ev1,ev2,ev3,ev4,ev5,ev6)

    expect_equivalent(allev, ev_expected)
    expect_setequal(names(allev), names(ev_expected))
})
