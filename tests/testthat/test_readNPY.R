context('Reading NPY files')

test_that('Numpy file is read correctly.', {
    filename <- 'data/testdata.npy'
    m1 <- readNPY(filename)
    m2 <- matrix(1:9, nrow=3, byrow = TRUE)
    expect_equal(m1, m2)
})
