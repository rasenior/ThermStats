#' batch_convert
#'
#' Batch convert list of raw FLIR matrices.
#' @param raw_dat A list of raw FLIR matrices returned from \code{batch_extract}.
#' @param out_dir Path to directory where converted temperature data will be written.
#' @return A list with one element per input thermal image. Each element is a numeric matrix of the
#' converted temperature data, in °C.
#' @examples
#'
#' # Setup --------------------------------------------------------------------
#'
#' # Load raw data
#' raw_dat <- flir_raw$raw_dat
#' camera_params <- flir_raw$camera_params
#' metadata <- flir_raw$metadata
#'
#' # Create vector denoting the position of each element of raw_dat (each photo)
#' # within the metadata dataframe
#' photo_index <- match(names(raw_dat), metadata$photo_no)
#'
#' # Define environmental parameters ------------------------------------------
#'
#' # Emissivity is mean of range given in Scheffers et al. 2017
#' # (onlinelibrary.wiley.com/doi/10.1111/btp.12355/full)
#' E <- mean(c(0.982,0.99))
#'
#' # Object distance is calculated as the hypotenuse of a right triangle where
#' # the vertical side is 1.3 m (breast height) and the angle down is 45 degrees
#' OD <- (sqrt(2))*1.3
#'
#' # Apparent reflected temperature, atmospheric temperature and infrared
#' # window temperature set as the atmospheric temperature measured in the field.
#' RTemp <- metadata$atm_temp[photo_index]
#' ATemp <- metadata$atm_temp[photo_index]
#' IRWTemp <- metadata$atm_temp[photo_index]
#'
#' # Infrared Window transmission is kept at default.
#' IRT <- 1
#'
#' # Relative humidity is set as the relative humidity measured in the field.
#' RH <- metadata$rel_humidity[photo_index]
#'
#' # Define camera parameters -------------------------------------------------
#'
#' # These parameters are calibration constants, and therefore constant for
#' # each camera
#' PR1 <- camera_params[,"PlanckR1"]
#' PB <- camera_params[,"PlanckB"]
#' PF <- camera_params[,"PlanckF"]
#' PO <- camera_params[,"PlanckO"]
#' PR2 <- camera_params[,"PlanckR2"]
#'
#' # Batch convert! -----------------------------------------------------------
#' results <- batch_convert(raw_dat, E, OD, RTemp, ATemp, IRWTemp, IRT, RH, PR1, PB, PF, PO, PR2, write_results = FALSE)
#' @export
#'
batch_convert <- function(raw_dat,
                          E = 1,
                          OD = 1,
                          RTemp = 20,
                          ATemp = RTemp,
                          IRWTemp = RTemp,
                          IRT = 1,
                          RH = 50,
                          PR1 = 21106.77,
                          PB = 1501,
                          PF = 1,
                          PO = -7340,
                          PR2 = 0.012545258,
                          write_results = TRUE,
                          out_dir = NULL,
                          file_name = NULL){

  # Apply to every element of the raw data list, converting raw data into
  # temperature using parameters from the metadata
  temp_dat <- mapply(FUN = Thermimage::raw2temp,
                     raw = raw_dat,
                     E,
                     OD,
                     RTemp,
                     ATemp,
                     IRWTemp,
                     IRT,
                     RH,
                     PR1,
                     PB,
                     PF,
                     PO,
                     PR2,
                     SIMPLIFY = FALSE)

  # Ensure correct element names
  names(temp_dat) <- names(raw_dat)

  # Write -------------------------------------------------------------------
  if(write_results){

    if(is.null(out_dir)) out_dir <- getwd()
    if(is.null(file_name)) file_name <- paste("flir_converted_", Sys.Date(),".Rds",sep="")

    out_path <- file.path(out_dir, paste(file_name, ".Rds", sep = ""))
    save(temp_dat,file = out_path)
  }
  return(temp_dat)

}

