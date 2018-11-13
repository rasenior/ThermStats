#' average_by_group
#'
#' Average multiple matrices or rasters within groups.
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
#' @export
#'
# Define function to return stats for each grouping
average_by_group <- function(metadata,
                             mat_list,
                             matrix_id,
                             grouping_var,
                             round_val){
    
    # Subset raster stack
    substack <- subset_stack(metadata = metadata, 
                             mat_list = mat_list,
                             matrix_id = matrix_id,
                             grouping_var = grouping_var, 
                             grouping_val = "PT1_1_0_1_1")
    if (requireNamespace("pbapply", quietly = TRUE)) {
        mat_avgs <-
            pbapply::pblapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      # Calculate mean for each group
                                      group_avg <- avg_stack(metadata = metadata, 
                                                             mat_list = mat_list,
                                                             matrix_id = matrix_id,
                                                             grouping_var = grouping_var, 
                                                             grouping_val = x)
                                      
                                      # Return
                                      return(group_avg)
                                      
                                  },
                                  error = function(err) {
                                      
                                      # error handler picks up where error was generated
                                      message(paste("\nMY_ERROR:  ",err))
                                      return(NA)
                                      
                                  })
                              })
    }else{
        mat_avgs <-
            lapply(unique(metadata[, grouping_var]),
                   function(x){
                       tryCatch({
                           # Calculate mean for each group
                           group_avg <- avg_stack(metadata = metadata, 
                                                  mat_list = mat_list,
                                                  matrix_id = matrix_id,
                                                  grouping_var = grouping_var, 
                                                  grouping_val = x)
                           
                           # Return
                           return(group_avg)
                           
                       },
                       error = function(err) {
                           
                           # error handler picks up where error was generated
                           message(paste("\nMY_ERROR:  ",err))
                           return(NA)
                           
                       })
                   })
    }
    
    
}

avg_stack <- function(metadata,
                      mat_list,
                      matrix_id,
                      grouping_var,
                      grouping_val){
    
    # Define matrix IDs for the group
    ids <- metadata[metadata[,grouping_var] == grouping_val, matrix_id]
    
    # Define the indices of these matrices in the matrix list
    inds <- which(gsub("[a-z]","",tolower(names(mat_list))) %in% ids)
    
    if(class(mat_list)[1] == "RasterStack"){
        # Subset
        substack <- mat_list[[inds]]
        # Create mean raster
        avg <- raster::mean(substack)
        # Name it
        names(avg) <- grouping_val
    }else{
        # Subset
        substack <- mat_list[inds]
        # Create mean matrix
        avg <- apply(simplify2array(substack), 1:2, mean)
    }
    
    # Return
    return(avg)
}


