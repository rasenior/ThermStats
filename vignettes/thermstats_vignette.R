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

