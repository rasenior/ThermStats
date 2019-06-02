context("handles no patches")

test_that("get_patches handles case where there are no patches", {
    set.seed(187)
    mat <- matrix(sample(1:2, 81, replace = TRUE), nrow = 9, ncol = 9)
    
    img_list <- batch_convert(flir_raw$raw_dat, write_results = FALSE)
    
    expect_false(any(get_patches(img = mat)$df$G_bin != 0))
    expect_true(get_patches(img = mat)$pstats$hot_area == 0)
    expect_true(get_patches(img = mat)$pstats$cold_area == 0)
    
})
