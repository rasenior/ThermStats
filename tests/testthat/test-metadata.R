context("metadata variables")

test_that("id variables present in metadata", {
    img_list <- batch_convert(flir_raw$raw_dat, write_results = FALSE)
    
    expect_error(average_by_group(flir_metadata, img_list = img_list, 
                                  id = "photoNo", grouping_var = "rep_id"),
                 "Given id variable is missing from metadata")
    expect_error(stats_by_group(flir_metadata, img_list = img_list,
                                idvar =  "photoNo", grouping_var = "rep_id"),
                 "Given id variable is missing from metadata")
    expect_error(average_by_group(flir_metadata, img_list = img_list, 
                                  id = "photo_no", grouping_var = "repID"),
                 "Given grouping variable is missing from metadata")
    expect_error(stats_by_group(flir_metadata, img_list = img_list, 
                                  idvar = "photo_no", grouping_var = "repID"),
                 "Given grouping variable is missing from metadata")
})