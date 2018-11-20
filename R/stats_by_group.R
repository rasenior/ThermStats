#' stats_by_group
#'
#' Calculate summary and spatial statistics across multiple matrices within groups.
#' @param metadata A dataframe denoting the grouping of different matrices.
#' @param mat_list List of matrices.
#' @param matrix_id Name of the metadata variable that identifies unique
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
#' metadata <- flir_metadata
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
#'                    matrix_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    style = "C",
#'                    mean, max, min)
#'
#' # Pixel stats = kurtosis and sknewness
#' patch_stats_2 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    style = "C",
#'                    kurtosis, skewness)
#'
#' # Pixel stats = 5th and 95th percentiles
#' patch_stats_3 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    style = "C",
#'                    perc_5, perc_95)
#'
#' # Pixel stats = Shannon and Simpson Diversity Indices
#' patch_stats_4 <-
#'     stats_by_group(metadata = metadata,
#'                    mat_list = mat_list,
#'                    matrix_id = "photo_no",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    style = "C",
#'                    SHDI, SIDI)
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(metadata,
                           mat_list,
                           matrix_id,
                           style = "C",
                           grouping_var,
                           round_val,
                           sum_stats = c("mean", "min","max")){
    
    # Determine if raster stack or list of matrices
    if(class(mat_list)[1] == "RasterStack"){
        mat_type <- "rasters"
    }else mat_type <- "matrices"
    
    if (requireNamespace("pbapply", quietly = TRUE)) {
        temp_stats<-
            pbapply::pblapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      sub_mat <- create_subset(metadata = metadata,
                                                               mat_list = mat_list,
                                                               matrix_id = matrix_id,
                                                               grouping_var = grouping_var,
                                                               grouping_val = x,
                                                               mat_type = mat_type,
                                                               round_val = round_val)
                                      n_mat <- sub_mat[["n_mat"]]
                                      sub_mat <- sub_mat[["sub_mat"]]
                                      
                                      # Get stats
                                      result <-
                                          get_stats(val_mat = sub_mat,
                                                    matrix_id = x,
                                                    calc_connectivity = FALSE,
                                                    conn_threshold = NULL,
                                                    get_patches = TRUE,
                                                    style = style,
                                                    mat_proj = NULL,
                                                    mat_extent = NULL,
                                                    return_vals = "pstats",
                                                    sum_stats = sum_stats
                                          )
                                      
                                      # Add number matrices
                                      result <- cbind(result, n_mat)
                                      # Return
                                      return(result)
                                      
                                  },
                                  error = function(err) {
                                      
                                      # error handler picks up where error was generated
                                      message(paste("\nMY_ERROR:  ",err))
                                      return(NA)
                                      
                                  })
                              })
    }else{
        temp_stats<-
            lapply(unique(metadata[, grouping_var]),
                   function(x){
                       tryCatch({
                           sub_mat <- create_subset(metadata = metadata,
                                                    mat_list = mat_list,
                                                    matrix_id = matrix_id,
                                                    grouping_var = grouping_var,
                                                    grouping_val = x,
                                                    mat_type = mat_type,
                                                    round_val = round_val)
                           n_mat <- sub_mat[["n_mat"]]
                           sub_mat <- sub_mat[["sub_mat"]]
                           
                           # Get stats
                           result <-
                               get_stats(val_mat = sub_mat,
                                         matrix_id = x,
                                         calc_connectivity = FALSE,
                                         conn_threshold = NULL,
                                         get_patches = TRUE,
                                         style = style,
                                         mat_proj = NULL,
                                         mat_extent = NULL,
                                         return_vals = "pstats",
                                         sum_stats = sum_stats
                               )
                           
                           # Add number matrices
                           result <- cbind(result, n_mat)
                           # Return
                           return(result)
                           
                       },
                       error = function(err) {
                           
                           # error handler picks up where error was generated
                           message(paste("\nMY_ERROR:  ",err))
                           return(NA)
                           
                       })
                   })
    }
    temp_stats <- do.call("rbind",temp_stats)
    return(temp_stats)
}


create_subset <- function(metadata,
                          mat_list,
                          matrix_id,
                          grouping_var,
                          grouping_val,
                          mat_type,
                          round_val){
    
    # Define matrix IDs for the group
    ids <- unique(metadata[metadata[,grouping_var] == grouping_val, matrix_id])
    
    # If raster stack need to paste 'X' onto numeric ids
    if(mat_type == "rasters" & is.numeric(ids)) ids <- paste("X", ids, sep = "")
    
    # Define the indices of these matrices in the matrix list
    inds <- which(names(mat_list) %in% ids)
    
    # If there are no matches, return NA
    if(length(inds) == 0){
        # Return
        return(NA) 
    }
    
    # Define the number of matrices  in this group
    n_mat <- length(ids)
    
    # Subset raster stack by the desired matrix IDs
    if(mat_type == "rasters"){
        sub_list <- mat_list[[inds]]
        # Unstack
        raster_list <- unstack(sub_list)
        
        # Subset matrices list by the desired matrix IDs
    }else{
        sub_list <- mat_list[inds]
        # Coerce any raster layers to matrices
        raster_list <-
            sub_list[which(sapply(sub_list, function(x) class(x) == "RasterLayer"))]
    }
    
    if(length(raster_list) > 0){
        raster_list <-
            lapply(raster_list, function(rast){
                # To dataframe first (seems to preserve coordinates more reliably)
                temp_df <- raster::as.data.frame(rast, xy = TRUE)
                # To matrix
                val <- names(temp_df)[3]
                temp_mat <- reshape2::acast(temp_df, y ~ x, value.var = val)
                return(temp_mat)
            })
        
        if(mat_type == "rasters"){
            sub_list <- raster_list
        }else{
            # Replace
            sub_list[which(sapply(sub_list, function(x) class(x) == "RasterLayer"))] <-
                raster_list
        }
    }
    
    # Round values to desired precision
    sub_list<-
        lapply(sub_list, function(x){
            round_val * round(x / round_val)
        })
    
    # Pad matrices with NAs (because they are not actually adjacent in space)
    sub_list <-
        lapply(sub_list, function(x){
            cbind(x, NA)
        })
    
    # Bind together
    sub_mat <- do.call("cbind", sub_list)
    
    return(list(sub_mat = sub_mat, n_mat = n_mat))
}

