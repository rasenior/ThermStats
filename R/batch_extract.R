#' batch_extract
#'
#' Batch extraction of raw data from FLIR thermal images.
#' @param in_dir Path to directory where thermal images are stored.
#' @param write_results Should the results be written as an Rdata file? Defaults to true.
#' @param out_dir Path to directory where output Rdata file will be stored. Defaults to working directory.
#' @return A list containing:
#'  \item{raw_dat}{A list with one element per input thermal image. Each element is a numeric matrix of the
#' raw infrared data.}
#'  \item{camera_params}{A dataframe of callibration constants unique to each camera.}
#' @examples
#' # Batch extract four FLIR thermal images included in this package.
#' results <- batch_extract(system.file("extdata", package = "PatchStatsFLIR"), write_results = FALSE)
#' @export
#'
batch_extract <- function(in_dir, write_results = TRUE, out_dir = NULL){

  # File names --------------------------------------------------------------

  # Get file names
  file.names <- list.files(in_dir, full.names = TRUE)

  # Remove path & file extension to get photo number
  photo_no <-
    gsub("FLIR","",file.names) %>%
    gsub(".jpg","",.) %>%
    gsub(paste("[", in_dir, "/]", sep = ""), "", .)

  # Create empty list to populate with temperature matrices
  raw_dat <- vector("list", length(photo_no))
  names(raw_dat) <- photo_no

  # Extract FLIR data -------------------------------------------------------

  for(i in 1:length(file.names)){
    cat("Processing file", i, "of", length(file.names),"\n")
    cat("Reading file...","\n")

    # Try and read in each FLIR file
    photo_i<-tryCatch(
      {
        Thermimage::readflirJPG(imagefile = file.names[i], exiftoolpath = "installed")
      },
      error=function(x){
        message(paste("Couldn't process file:",file.names[i]))
        return(NA)
      })


    # Flip the matrix (makes plotting later on easier)
    photo_i <- Thermimage::mirror.matrix(Thermimage::rotate180.matrix(photo_i))
    colnames(photo_i)<-NULL

    # Write the matrix to the appropriate index in the empty list
    raw_dat[[i]] <- photo_i
  }

  # Get camera parameters (constant for each camera)
  cat("Extracting camera parameters...","\n")
  camera_params <-
    Thermimage::flirsettings(imagefile = file.names[1],
                             exiftoolpath = "installed")

  # Reduce to parameters of interest
  camera_params <-
    unlist(camera_params$Info[c("photo_no","PlanckR1","PlanckB",
                                "PlanckF","PlanckO","PlanckR2")[2:6]])

  camera_params <- as.data.frame(t(camera_params))

  # Write -------------------------------------------------------------------

  results <- list(raw_dat = raw_dat, camera_params = camera_params)

  if(write_results){

    if(is.null(out_dir)) out_dir <- getwd()
    date <- Sys.Date()
    save(results,file = file.path(out_dir,paste("flir_raw_",
                                                date,".Rdata",sep="")))
  }

  return(results)
}





