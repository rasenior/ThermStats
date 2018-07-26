## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.align = "centre", fig.width = 16.6/2.54)

## ----install, echo = TRUE, eval = -1-------------------------------------
devtools::install_github("rasenior/ThermStats")
library(ThermStats)

## ----batch-extract, echo= TRUE, results = "hide"-------------------------
# Batch extract four FLIR thermal images included 
# in the ThermStats package installation
flir_raw <-
   batch_extract(in_dir = 
                     system.file("extdata",
                                 package = 
                                     "ThermStats"),
                 write_results = FALSE)

## ----batch-convert, echo= TRUE, results = "hide"-------------------------
# Define raw data
raw_dat <- flir_raw$raw_dat
# Define camera calibration constants dataframe
camera_params <- flir_raw$camera_params
# Define metadata
metadata <- flir_metadata

# Create vector denoting the position of each 
# photo within the metadata dataframe
photo_index <- match(names(raw_dat), 
                     metadata$photo_no)

# Batch convert ---------------------------------
flir_converted <-
    batch_convert(
        raw_dat = raw_dat,
        # Emissivity = mean of the range in 
        # Scheffers et al. 2017
        E = mean(c(0.982,0.99)),
        # Object distance = hypotenuse of a right
        # triangle where the vertical side is 
        # 1.3 m (breast height) & the angle down 
        # is 45 degrees
        OD = (sqrt(2))*1.3,
        # Apparent reflected temperature, 
        # atmospheric temperature and infrared 
        # window temperature set as the 
        # atmospheric temperature measured in 
        # the field
        RTemp = metadata$atm_temp[photo_index],
        ATemp = metadata$atm_temp[photo_index],
        IRWTemp = metadata$atm_temp[photo_index],
        # Infrared Window transmission kept at 
        # default value of 1
        IRT = 1,
        # Relative humidity is set as the 
        # relative humidity measured in the field
        RH = metadata$rel_humidity[photo_index],
        # Calibration constants from 
        # batch_extract, constant for each camera
        PR1 = camera_params[,"PlanckR1"],
        PB = camera_params[,"PlanckB"],
        PF = camera_params[,"PlanckF"],
        PO = camera_params[,"PlanckO"],
        PR2 = camera_params[,"PlanckR2"],
        # Whether to write results or just return
        write_results = FALSE)

## ----get-stats, echo= TRUE, results = "hide"-----------------------------
flir_stats <-
    get_stats(
        # The temperature matrix 
        val_mat = flir_converted$`8565`,
        # The ID of the matrix
        matrix_id = "8565",
        # Whether or not to identify hot and 
        # cold spots
        get_patches = TRUE,
        # The size of the neighourhood 
        # (for calculating local G statistic)
        k = 8,
        # The neghbour weighting style 
        # (for calculating local G statistic)
        style = "W",
        # The matrix projection
        # (only relevant for geographic data)
        mat_proj = NULL,
        # The matrix extent
        # (only relevant for geographic data)
        mat_extent = NULL,
        # The data to return
        return_vals = c("df", 
                        "patches", 
                        "pstats"),
        # The names of the statistics functions
        pixel_fns = NULL,
        # The summary statistics
        median, perc_5, perc_95, SHDI
    )

## ----tab-B-1, results='asis'---------------------------------------------
tab1 <- "
Table: Example metadata denoting the grouping ('rep_id') of different temperature matrices. Statistics can be calculated over multiple matrices within a group, using the function `stats_by_group`.

| photo_no|rep_id | atm_temp| rel_humidity|
|--------:|:------|--------:|------------:|
|     8565|T7P1   |    24.00|           96|
|     8583|T7P1   |    24.00|           96|
|     8589|T7P2   |    23.25|           98|
|     8613|T7P2   |    23.50|           96|
"

cat(tab1) # output the table in a format good for HTML/PDF/docx conversion

## ----stats-by-group, results = "hide"------------------------------------
flir_stats_group <-
    stats_by_group(
        # A dataframe denoting the grouping
        metadata = metadata,
        # List of temperature matrices
        mat_list = flir_converted,
        # Variable denoting the matrix IDs
        matrix_id = "photo_no",
        # Variable denoting the grouping
        grouping_var = "rep_id",
        # Desired precision of data
        round_val = 0.5,
        # The size of the neighourhood 
        # (for calculating local G statistic)
        k = 8,
        # The neghbour weighting style 
        # (for calculating local G statistic)
        style = "W",
        # Some summary statistics
        median, perc_5, perc_95, SHDI
        )

## ----tab-B-2, results='asis'---------------------------------------------
tab2 <- "
Table: A snippet of hot spot patch statistics returned by `stats_by_group`, which implements `get_stats` within groups.

| median| perc_5| perc_95|     SHDI| hot_shape_index| hot_aggregation|
|------:|------:|-------:|--------:|---------------:|---------------:|
|   23.5|     23|    24.5|     1.16|            7.54|           0.895|
|   24.0|     23|    25.0|     1.68|            7.80|           0.855|
"

cat(tab2) # output the table in a format good for HTML/PDF/docx conversion

## ----fig-B-1, fig.cap= "The output of 'plot_patches' includes a histogram and the original temperature data overlaid with outlines of hot and cold spots, identified using the Getis-Ord local statistic."----
plot_patches(
    # The raw temperature data
    df = flir_stats$df,
    # The patch outlines
    patches = flir_stats$patches
        )

