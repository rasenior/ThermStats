#' stats_by_group
#'
#' Calculate summary and spatial statistics across multiple matrices within groups.
#' @param metadata A dataframe denoting the grouping of different matrices.
#' @param mat_list List of matrices.
#' @param element_id Name of the metadata variable that identifies unique
#' matrices. Should match element names in the list of matrices.
#' @param k Number of neighbours to use when calculating nearest neighbours
#' using \code{spdep::}\code{\link[spdep]{knearneigh}}.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}.
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
#'                    element_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    mean, max, min)
#'
#' # Pixel stats = kurtosis and sknewness
#' patch_stats_2 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    element_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    kurtosis, skewness)
#'
#' # Pixel stats = 5th and 95th percentiles
#' patch_stats_3 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    element_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    perc_5, perc_95)
#'
#' # Pixel stats = Shannon and Simpson Diversity Indices
#' patch_stats_4 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    element_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    SHDI, SIDI)
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(metadata,
                           mat_list,
                           element_id,
                           k = 8,
                           style = "W",
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
                                              element_id = element_id,
                                              k = k,
                                              style = style,
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
                                   element_id = element_id,
                                   k = k,
                                   style = style,
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
get_stats <- function(metadata,
                      mat_list,
                      element_id,
                      k = 8,
                      style = "W",
                      grouping_var,
                      grouping_val,
                      round_val = 1,
                      pixel_fns = NULL,...){

  # Setup ----------------------------------------------------------------------
  sub_photos <-
    as.character(metadata[metadata[,grouping_var]==grouping_val,element_id])

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

  pixel_stats <- multi_sapply(sub_mat,...)

  colnames(pixel_stats) <- pixel_fns

  # Patch statistics -----------------------------------------------------------
  # -> these statistics are calculated for hot and cold patches

  patch_stats <- get_patches(val_mat = sub_mat,
                             matrix_id = grouping_val,
                             k = k,
                             style = style,
                             mat_proj = NULL,
                             mat_extent = NULL,
                             return_vals = "pstats")

  # Return results -------------------------------------------------------------

  # Bind all results
  results <- cbind(n_photos, pixel_stats, patch_stats)
  # Add ID
  results[,grouping_var] <- grouping_val

  return(results)
}
