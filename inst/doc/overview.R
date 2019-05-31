## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.align = "centre", fig.width = 16.6/2.54)

## ----fig-1, fig.cap= "Figure 1: Schematic summarising the key functions for processing groups of images (left) or a single image (right).", fig.pos = 'H', out.width = "400px"----
knitr::include_graphics("schematic.png")

## ----install, echo = TRUE, eval = FALSE----------------------------------
#  devtools::install_github("rasenior/ThermStats")

## ----load, echo = TRUE---------------------------------------------------
library(ThermStats)

## ----batch-extract, echo= TRUE, eval = FALSE-----------------------------
#  # Batch extract thermal images included in ThermStats installation
#  flir_raw <-
#      batch_extract(in_dir = system.file("extdata",
#                                         package = "ThermStats"),
#                    write_results = FALSE)

## ----batch-convert, echo = TRUE, results = "hide"------------------------
# Define raw data
raw_dat <- flir_raw$raw_dat
# Define camera calibration constants dataframe
camera_params <- flir_raw$camera_params
# Define metadata
metadata <- flir_metadata
# Create vector denoting the position of each photo within metadata
photo_index <- match(names(raw_dat), 
                     metadata$photo_no)
# Batch convert
flir_converted <-
    batch_convert(
        raw_dat = raw_dat,
        # Emissivity = mean of range in Scheffers et al. 2017
        E = mean(c(0.982,0.99)),
        # Object distance = hypotenuse of right triangle where 
        # vertical side is 1.3 m (breast height) & angle down is 45°
        OD = (sqrt(2))*1.3,
        # Apparent reflected temperature & atmospheric temperature =
        # atmospheric temperature measured in the field
        RTemp = metadata$atm_temp[photo_index],
        ATemp = metadata$atm_temp[photo_index],
        # Relative humidity = relative humidity measured in the field
        RH = metadata$rel_humidity[photo_index],
        # Calibration constants from 'batch_extract'
        PR1 = camera_params[,"PlanckR1"],
        PB = camera_params[,"PlanckB"],
        PF = camera_params[,"PlanckF"],
        PO = camera_params[,"PlanckO"],
        PR2 = camera_params[,"PlanckR2"],
        # Whether to write results or just return
        write_results = FALSE)

## ----get-stats, echo= TRUE, eval = FALSE---------------------------------
#  flir_stats <-
#      get_stats(
#          # The temperature dataset
#          img = flir_converted$`8565`,
#          # The ID of the dataset
#          id = "8565",
#          # Whether or not to calculate thermal connectivity
#          calc_connectivity = FALSE,
#          # Whether or not to identify hot and cold spots
#          patches = TRUE,
#          # The image projection (only relevant for geographic data)
#          img_proj = NULL,
#          # The image extent (only relevant for geographic data)
#          img_extent = NULL,
#          # The data to return
#          return_vals = c("df", # Temperature data as dataframe
#                          "patches", # Patch outlines
#                          "pstats"), # Patch statistics dataframe
#          # The summary statistics of interest
#          sum_stats = c("median", "SHDI",
#                        "perc_5", "perc_95"))

## ----tab-1, results='asis'-----------------------------------------------
tab1 <- "
Table: Table 1: Example metadata denoting the grouping ('rep_id') of different thermal images. Statistics can be calculated over multiple images within a group, using the function `stats_by_group`.

| photo_no|rep_id | atm_temp| rel_humidity|
|--------:|:------|--------:|------------:|
|     8565|T7P1   |    24.00|           96|
|     8583|T7P1   |    24.00|           96|
|     8589|T7P2   |    23.25|           98|
|     8613|T7P2   |    23.50|           96|
"

cat(tab1)

## ----stats-by-group, eval = FALSE----------------------------------------
#  flir_stats_group <-
#      stats_by_group(
#          # A dataframe denoting the grouping
#          metadata = metadata,
#          # List of images
#          img_list = flir_converted,
#          # Variable denoting the ID of unique images
#          idvar = "photo_no",
#          # Variable denoting the grouping
#          grouping_var = "rep_id",
#          # Desired precision of data
#          round_val = 0.5,
#          # The data to return
#          return_vals = c("df", # Temperature data as dataframe
#                          "patches", # Patch outlines
#                          "pstats"), # Patch statistics dataframe
#          # The summary statistics of interest
#          sum_stats = c("median", "SHDI",
#                        "perc_5", "perc_95"))

## ----tab-2, results='asis'-----------------------------------------------
tab2 <- "
Table: Table 2: A snippet of hot spot patch statistics returned by `stats_by_group`, which implements `get_stats` within groups.

| img_median| img_perc_5| img_perc_95|     img_SHDI| hot_shape_index| hot_aggregation|
|----------:|----------:|-----------:|------------:|---------------:|---------------:|
|       23.5|         23|        24.5|         1.16|            7.54|           0.895|
|       24.0|         23|        25.0|         1.68|            7.80|           0.855|
"

cat(tab2)

## ----fig-2, fig.cap= "Figure 2: The output of `plot_patches` includes a histogram and the original temperature data overlaid with outlines of hot and cold spots, identified using the G* variant of the Getis-Ord local statistic.", echo = TRUE----
plot_patches(
    # The raw temperature data
    df = flir_stats$df,
    # The patch outlines
    patches = flir_stats$patches)

