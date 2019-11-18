context('applying duration threshold')

## Function to generate test vectors.  The index tells us which case
## to generate.  Update ncase if we add cases.
ncase <- 6
vecs <- function(i)
{
    ## All of the comments on the cases assume thresh==6
    switch(i,
        c(0,0,0, seq(1,5), 0,0,0, seq(1,6), 0, 0, 0),  # one to drop and one to keep
        c(seq(1,6), 0, seq(1,5), 0, seq(1,7)),         # keeper at the beginning and end
        c(seq(1,5), 0, seq(1,8), 0, seq(1,5)),         # drop at beginning and end
        seq(1,20),                                     # one long sequence
        c(1, rep(0, 19)),                              # single event in the first timestep
        rep(0, 20)                                     # no events
    )
}

answers <- function(i)
{
    ## All answers assume that thresh == 6
    switch(i,
           c(rep(0,11), seq(1,6), rep(0,3)),
           c(seq(1,6), 0, rep(0,5), 0, seq(1,7)),
           c(rep(0,5), 0, seq(1,8), 0, rep(0,5)),
           seq(1,20),
           rep(0, 20),
           rep(0,20)
    )
}

test_that('apply_duration_thresh works for normal and edge cases.', {
    for(i in seq(1,ncase)) {
        v <- vecs(i)
        v2 <- apply_duration_thresh(v, 6)
        expect_equal(v2, answers(i), info=paste('input vector ', i))
        ## apply_duration_thresh modifies its argument!
        expect_equal(v, v2, info=paste('input vector ', i))
    }
    ## Test the case where there is just one sequence and it needs to be dropped.
    v <- vecs(4)
    v2 <- apply_duration_thresh(v, 21)
    expect_equal(v2, rep(0,20))
})

test_that('apply_duration_thresh can be applied row-wise to a matrix.', {
    m <- t(sapply(1:ncase, vecs))
    m2 <- apply(m, 1, apply_duration_thresh, thresh=6)    # answers will be in colums
    ma <- sapply(1:ncase, answers)                        # no need for transpose

    expect_equal(m2, ma)

    ## Check that m was not modified.
    expect_equal(m, t(sapply(1:ncase, vecs)))
})
