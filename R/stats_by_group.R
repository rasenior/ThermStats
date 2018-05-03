#' stats_by_group
#'
#' Calculate thermal statistics across photos within groups.
#' @param metadata A dataframe denoting the grouping of different matrices.
#' @param mat_list List of temperatures matrices.
#' @param matrix_ID Name of the metadata variable that identifies unique
#' temperature matrices. Should match element names in the list of matrices.
#' @param grouping_var The name of the metadata variable that denotes the grouping
#' of temperature matrices.
#' @param round_val Value to round to. Use 1 for no rounding.
#' @param ... Use to specify statistics that should be calculated across all
#' pixels. Several helper functions are included for use here: perc_5, perc_95,
#' SHDI, SIDI. See examples below.
#' @return A list containing:
#'  \item{raw_dat}{A list with one element per input thermal image. Each element is a numeric matrix of the
#' raw infrared data.}
#'  \item{camera_params}{A dataframe of callibration constants unique to each camera.}
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
#' # Specifying mean, max and min for pixel stats
#' patch_stats_1 <- stats_by_group(metadata = metadata, mat_list = mat_list, matrix_ID = "photo_no", grouping_var = "rep_id", round_val = 0.5, mean, max, min)
#'
#' # Specifying kurtosis and sknewness for pixel stats
#' library(moments)
#' patch_stats_2 <- stats_by_group(metadata = metadata, mat_list = mat_list, matrix_ID = "photo_no", grouping_var = "rep_id", round_val = 0.5, kurtosis, skewness)
#'
#' # Specifying 5th and 95th percentiles for pixel stats
#' patch_stats_3 <- stats_by_group(metadata = metadata, mat_list = mat_list, matrix_ID = "photo_no", grouping_var = "rep_id", round_val = 0.5, perc_5, perc_95)
#'
#' # Specifying Shannon and Simpson Diversity Indices for pixel stats
#' patch_stats_4 <- stats_by_group(metadata = metadata, mat_list = mat_list, matrix_ID = "photo_no", grouping_var = "rep_id", round_val = 0.5, SHDI, SIDI)
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
      pblapply(unique(metadata[, grouping_var]),
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
                      pixel_fns,...){

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

  # Pixel statistics -----------------------------------------------------------
  # -> these statistics are calculated across all pixels of all subset photos

  # Bind all data together
  all_pixels <- as.vector(do.call("rbind",sub_list))

  pixel_stats <- multi.sapply(all_pixels,...)

  colnames(pixel_stats) <- pixel_fns

  # Patch statistics -----------------------------------------------------------
  # -> these statistics are calculated for hot and cold patches

  # For each matrix in the list, calculate spatial statistics
  patch_stats <-
    lapply(1:length(sub_list),
           function(x) get_patches(mat =  sub_list[[x]],
                                   matrix_id = sub_photos[x],
                                   return_vals = "patch_stats",
                                   k = 8,
                                   style = "W"))

  # Bind as dataframe
  patch_stats <- do.call("rbind", patch_stats)

  ### Aggregate results over all photos to give one statistic for the subset

  # Define variables that should be summed across photos
  sum_vars <-
    c("hot_px_no","hot_patch_no","hot_obs_edges","hot_max_edges","hot_non_edges",
      "cold_px_no","cold_patch_no","cold_obs_edges","cold_max_edges","cold_non_edges")

  patch_stats <-
    c(apply(patch_stats[,sum_vars],
            MARGIN = 2,
            FUN = sum,
            na.rm=TRUE),
      # Max patch temp is the max across all photos
      apply(patch_stats[,c("hot_patch_max_val",
                           "cold_patch_max_val")],
            MARGIN = 2,
            FUN = max,
            na.rm=TRUE),
      # Min patch temp is the min across all photos
      apply(patch_stats[,c("hot_patch_min_val",
                           "cold_patch_min_val")],
            MARGIN = 2,
            FUN = min,
            na.rm=TRUE))
  # Reformat
  patch_stats <- data.frame(t(patch_stats))

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

