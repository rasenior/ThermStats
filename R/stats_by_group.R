#' stats_by_group
#'
#' Calculate summary and spatial statistics across multiple matrices within groups.
#' @param metadata A dataframe denoting the grouping of different matrices.
#' @param mat_list List of matrices.
#' @param matrix_ID Name of the metadata variable that identifies unique
#' matrices. Should match element names in the list of matrices.
#' @param grouping_var The name of the metadata variable that denotes the
#' grouping of matrices.
#' @param round_val Value to round to. Use 1 for no rounding.
#' @param ... Use to specify summary statistics that should be calculated across
#' all pixels. Several helper functions are included for use here:
#' \code{\link{perc_5}}, \code{\link{perc_95}},
#' \code{\link{SHDI}}, \code{\link{SIDI}},
#' \code{\link{kurtosis}} and \code{\link{skewness}}. Also see examples below.
#' @return A dataframe with pixel and patch statistics. See
#' \code{\link{patch_stats}} for details of all the patch statistics returned.
#' @examples
#' # Load raw data
#' raw_dat <- flir_raw$raw_dat
#' camera_params <- flir_raw$camera_params
#' metadata <- flir_raw$metadata
#'
#' # Batch convert
#' mat_list <- batch_convert(raw_dat, write_results = FALSE)
#'
#' # Calculate patch and pixel stats! ------------------------------------------
#'
#' # Pixel stats = mean, max and min
#' patch_stats_1 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_ID = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    mean, max, min)
#'
#' # Pixel stats = kurtosis and sknewness
#' patch_stats_2 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_ID = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    kurtosis, skewness)
#'
#' # Pixel stats = 5th and 95th percentiles
#' patch_stats_3 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_ID = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    perc_5, perc_95)
#'
#' # Pixel stats = Shannon and Simpson Diversity Indices
#' patch_stats_4 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_ID = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    SHDI, SIDI)
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(metadata,mat_list,matrix_ID,
                           grouping_var, round_val,...){

  # Define function names for pixel stats
  pixel_fns <-
    paste(match.call(expand.dots = FALSE)$...)

  if (requireNamespace("pbapply", quietly = TRUE)) {
    temp_stats<-
      pbapply::pblapply(unique(metadata[, grouping_var]),
                        function(x){
                          tryCatch({get_stats(metadata = metadata,
                                              mat_list = mat_list,
                                              matrix_ID = matrix_ID,
                                              grouping_var = grouping_var,
                                              grouping_val = x,
                                              round_val = round_val,
                                              pixel_fns = pixel_fns,
                                              ...)},
                                   error=function(cond)NA)
                        })
  }else{
    temp_stats<-
      lapply(unique(metadata[, grouping_var]),
             function(x){
               tryCatch({get_stats(metadata = metadata,
                                   mat_list = mat_list,
                                   matrix_ID = matrix_ID,
                                   grouping_var = grouping_var,
                                   grouping_val = x,
                                   round_val = round_val,
                                   pixel_fns = pixel_fns,
                                   ...)},
                        error=function(cond)NA)
             })
  }

  temp_stats <- do.call("rbind",temp_stats)
  return(temp_stats)
}


# Define function to return stats for each grouping
get_stats <- function(metadata,mat_list,matrix_ID,
                      grouping_var,grouping_val, round_val = 1,
                      pixel_fns = NULL,...){

  # Setup ----------------------------------------------------------------------
  sub_photos <-
    as.character(metadata[metadata[,grouping_var]==grouping_val,matrix_ID])

  n_photos <- length(sub_photos)

  # Subset matrices list by the desired photo numbers
  sub_list <- mat_list[as.character(sub_photos)]

  # Rounding
  sub_list<-
    lapply(sub_list, function(x){
      round_val * round(x / round_val)
    })

  # Pad matrices with NAs (not adjacent in space)
  sub_list<-
    lapply(sub_list, function(x){
      cbind(x, NA)
    })

  # Bind together
  sub_mat <- do.call("cbind", sub_list)

  # Pixel statistics -----------------------------------------------------------
  # -> these statistics are calculated across all pixels of all subset photos

  pixel_stats <- multi.sapply(sub_mat,...)

  colnames(pixel_stats) <- pixel_fns

  # Patch statistics -----------------------------------------------------------
  # -> these statistics are calculated for hot and cold patches

  # For each matrix in the list, calculate spatial statistics
  patch_stats <- get_patches(val_mat = sub_mat,
                             return_vals = "pstats",
                             k = 8,
                             style = "W")

  # Return results -------------------------------------------------------------

  # Bind all results
  results <- cbind(n_photos, pixel_stats, patch_stats)
  # Add ID
  results[,grouping_var] <- grouping_val

  return(results)
}

# Function to apply multiple functions to a vector
multi.sapply <- function(...) {
  # Reads in all arguments passed to function, including data
  arglist <- match.call(expand.dots = FALSE)$...

  # Deparse argument names
  var.names <- sapply(arglist, deparse)

  # For all arguments that had function names specified,
  # substitue name from deparsed expression by the given name
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]

  # Evaluate the expressions given in arguments;
  # go two generations back as we apply eval.parent
  # within lapply function
  arglist <- lapply(arglist, eval.parent, n = 2)

  # First argument contains data set, so remove it from the list
  data <- arglist[[1]]
  arglist[[1]] <- NULL

  # Apply every function
  val<-sapply(X=arglist,
              FUN=function(arglist) arglist(data, na.rm = TRUE))

  # Re-format to dataframe
  val <- as.data.frame(t(val))
  rownames(val) <- NULL

  return(val = val)
}
