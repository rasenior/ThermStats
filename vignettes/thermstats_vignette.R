## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install-------------------------------------------------------------
devtools::install_github("rasenior/ThermStats")
library(ThermStats)

## ----batch-extract, echo= TRUE, results = "hide"-------------------------
# Batch extract four FLIR thermal images included in ThermStats
flir_raw <-
    batch_extract("~./../Desktop/extdata",
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

