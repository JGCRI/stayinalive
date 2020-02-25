context('Reading NPY files')

test_that('Numpy file is read correctly.', {
    filename <- 'data/testdata.npy'
    m1 <- readNPY(filename)
    m2 <- matrix(1:9, nrow=3, byrow = TRUE)
    expect_equal(m1, m2)
})

test_that('Pickle file is read correctly.', {
    filename1 <- 'data/testdata.pkl'
    l1 <- readpkl(filename1)
    expect_is(l1, 'list')
    expect_true(all(sapply(l1, is.matrix)))
    l2 <- list(matrix(0, nrow=2, ncol=2),
            matrix(1, nrow=2, ncol=3),
            matrix(23, nrow=3, ncol=3))
    expect_equal(l1, l2)

    ## With no sublist arg, we can read an arbitrary python data structure.
    filename2 <- 'data/testdata-nolist.pkl'
    m3 <- readpkl(filename2)
    expect_equal(m3, matrix(23, nrow=3, ncol=3))
})

test_that('Reading a sublist from a pickle file works.', {
    filename1 <- 'data/testdata.pkl'
    l1 <- readpkl(filename1, 1:2)
    l2 <- list(matrix(0, nrow=2, ncol=2),
               matrix(1, nrow=2, ncol=3))
    expect_equal(l1, l2)

    ## reading something that is not a list should fail
    filename2 <- 'data/testdata-nolist.pkl'
    expect_error(l1a <- readpkl(filename2, 1:2), 'is.list\\(rtn\\) is not TRUE')
})
