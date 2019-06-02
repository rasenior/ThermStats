context("input raster or matrix")

test_that("raster or matrix give same result", {
    mat <- matrix(sample(1:5, 81, replace = TRUE), nrow = 9, ncol = 9)
    rast <- raster::raster(mat)
    
    # Get patches
    expect_equal(mean(get_patches(img = mat)$df$val),
                 mean(get_patches(img = rast)$df$val))
    expect_equal(get_patches(img = mat)$pstats,
                 get_patches(img = rast)$pstats)
    
    # Get stats
    expect_equal(get_stats(img = mat, 
                           calc_connectivity = TRUE, 
                           return_vals = "pstats", 
                           sum_stats = c("mean", "min","max")),
                 get_stats(img = rast, 
                           calc_connectivity = TRUE, 
                           return_vals = "pstats", 
                           sum_stats = c("mean", "min","max")))
})

test_that("raster stack or matrix list give same result", {
    mat <- matrix(sample(1:5, 81, replace = TRUE), nrow = 9, ncol = 9)
    img_list <- list(mat, mat, mat, mat)
    names(img_list) <- c("a","b")
    rast_stack <- stack_imgs(img_list)
    metadata <- data.frame(img_id = c("a", "b"),
                           rep_id = c("rep1", "rep2"))
    
    # stats by group
    expect_equal(mean(stats_by_group(img_list = img_list,
                                     metadata = metadata, 
                                     idvar = "img_id",
                                     grouping_var = "rep_id",
                                     sum_stats = c("mean", "min","max"))$df$val),
                 mean(stats_by_group(img_list = rast_stack,
                                     metadata = metadata, 
                                     idvar = "img_id",
                                     grouping_var = "rep_id",
                                     sum_stats = c("mean", "min","max"))$df$val))
    expect_equal(stats_by_group(img_list = img_list,
                                     metadata = metadata, 
                                     idvar = "img_id",
                                     grouping_var = "rep_id",
                                     sum_stats = c("mean", "min","max"))$pstats,
                 stats_by_group(img_list = rast_stack,
                                     metadata = metadata, 
                                     idvar = "img_id",
                                     grouping_var = "rep_id",
                                     sum_stats = c("mean", "min","max"))$pstats)
})