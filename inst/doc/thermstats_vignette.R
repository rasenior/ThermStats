## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE,
                      fig.align = "centre", fig.width = 8/2.54)

## ----install, eval = -1--------------------------------------------------
devtools::install_github("rasenior/ThermStats")
library(ThermStats)

## ----batch-extract, echo= TRUE, results = "hide"-------------------------
# Batch extract four FLIR thermal images included in ThermStats
flir_raw <-
  # NOTE this step may not work if there are spaces in the path to 
  # where the package is installed
   batch_extract(system.file("extdata", package = "ThermStats"),
                 write_results = FALSE)

## ----batch-convert, echo= TRUE, results = "hide"-------------------------
# Load raw data
raw_dat <- flir_raw$raw_dat
camera_params <- flir_raw$camera_params
metadata <- flir_metadata

# Create vector denoting the position of each photo within 
# the metadata dataframe
photo_index <- match(names(raw_dat), metadata$photo_no)

# Define environmental parameters -------------------------

# Emissivity is mean of range given in Scheffers et al. 2017
E <- mean(c(0.982,0.99))

# Object distance is calculated as the hypotenuse of a 
# right triangle where the vertical side is 1.3 m (breast 
# height) and the angle down is 45 degrees
OD <- (sqrt(2))*1.3

# Apparent reflected temperature, atmospheric temperature 
# and infrared window temperature set as the atmospheric 
# temperature measured in the field
RTemp <- metadata$atm_temp[photo_index]
ATemp <- metadata$atm_temp[photo_index]
IRWTemp <- metadata$atm_temp[photo_index]

# Infrared Window transmission is kept at default
IRT <- 1

# Relative humidity is set as the relative humidity 
# measured in the field.
RH <- metadata$rel_humidity[photo_index]

# Define camera parameters --------------------------------

# These parameters are calibration constants, and therefore 
# constant for each camera
PR1 <- camera_params[,"PlanckR1"]
PB <- camera_params[,"PlanckB"]
PF <- camera_params[,"PlanckF"]
PO <- camera_params[,"PlanckO"]
PR2 <- camera_params[,"PlanckR2"]

# Batch convert -------------------------------------------
flir_converted <-
    batch_convert(raw_dat,
                  E, OD, RTemp, ATemp, IRWTemp, IRT, RH,
                  PR1, PB, PF, PO, PR2,
                  write_results = FALSE)

## ----get-stats, echo= TRUE, results = "hide"-----------------------------
flir_stats <-
    get_stats(
        # The temperature matrix 
        val_mat = flir_converted$`8565`,
        # The ID of the matrix
        matrix_id = "8565",
        # Whether or not to identify hot and cold spots
        get_patches = TRUE,
        # The size of the neighourhood 
        # (for calculating the local G statistic)
        k = 8,
        # The neghbour weighting style 
        # (for calculating the local G statistic)
        style = "W",
        # The matrix projection
        # (only relevant for geographic data)
        mat_proj = NULL,
        # The matrix extent
        # (only relevant for geographic data)
        mat_extent = NULL,
        # The data to return
        return_vals = c("df", "patches", "pstats"),
        # The names of the statistics functions
        pixel_fns = NULL,
        # The summary statistics
        median, perc_5, perc_95, SHDI
    )

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
        # (for calculating the local G statistic)
        k = 8,
        # The neghbour weighting style 
        # (for calculating the local G statistic)
        style = "W",
        # Some summary statistics
        median, perc_5, perc_95, SHDI
        )

## ----tab-B-1, tidy=FALSE-------------------------------------------------
knitr::kable(
  head(flir_stats_group[, c(1:5,8,13:17)], 10), booktabs = TRUE,
  caption = "A snippet of hot spot patch statistics returned by 'stats_by_group', which implements 'get_stats' within groups."
)

## ----plot-patches, echo= TRUE, results = "hide"--------------------------
plot_patches(
    # The raw temperature data
    df = flir_stats$df,
    # The patch outlines
    patches = flir_stats$patches
        )

