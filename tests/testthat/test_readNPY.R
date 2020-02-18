context('Reading NPY files')

test_that('Numpy file is read correctly.', {
    filename <- 'data/testdata.npy'
    m1 <- readNPY(filename)
    m2 <- matrix(1:9, nrow=3, byrow = TRUE)
    expect_equal(m1, m2)
})

test_that('Pickle file is read correctly.', {
    filename <- 'data/testdata.pkl'
    l1 <- readpkl(filename)
    expect_is(l1, 'list')
    expect_true(all(sapply(l1, is.matrix)))
    l2 <- list(matrix(0, nrow=2, ncol=2),
            matrix(1, nrow=2, ncol=3))
    expect_equal(l1, l2)
})
