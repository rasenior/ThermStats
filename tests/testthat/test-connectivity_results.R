context("connectivity output")

test_that("dimensions of output results match input", {
    mat <- matrix(sample(1:2, 81, replace = TRUE), nrow = 9, ncol = 9)
    
    expect_equal(nrow(connectivity(img = mat)), length(mat))
})