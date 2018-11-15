#' average_by_group
#'
#' Average multiple matrices or rasters within groups.
#' @param metadata A dataframe denoting the grouping of different matrices.
#' @param mat_list List of matrices or raster stack.
#' @param matrix_id Name of the metadata variable that identifies unique
#' matrices. Should match element names in the list of matrices.
#' @param grouping_var The name of the metadata variable that denotes the
#' grouping of matrices.
#' @param round_val Number of decimals to round average matrices to (optional).
#' @return 
#' If \code{mat_list} is supplied as a list of matrices, returns
#' a new list of matrices with one element for each unique value of the grouping
#' variable. 
#' 
#' If \code{mat_list} is supplied as a raster stack, returns a new raster
#' stack with one raster for each unique value of the grouping variable. 
#' 
#' Grouping variables with no matching matrices are dropped.
#' @examples
#' # Load raw data
#' raw_dat <- flir_raw$raw_dat
#' camera_params <- flir_raw$camera_params
#' metadata <- flir_metadata
#'
#' # Batch convert
#' mat_list <- batch_convert(raw_dat, write_results = FALSE)
#' 
#' # With list of matrices -----------------------------------------------------
#' 
#' # Average matrices in list
#' avg_list <- average_by_group(metadata = metadata,
#'                              mat_list = mat_list,
#'                              matrix_id = "photo_no",
#'                              grouping_var = "rep_id")
#'                              
#' # With raster stack ---------------------------------------------------------
#' 
#' # Coerce each matrix to a raster
#' mat_stack <-
#'     lapply(mat_list,
#'            function(x) raster::raster(x,
#'                                       xmn=0, xmx=ncol(mat_list[[1]]),
#'                                       ymn=0, ymx=nrow(mat_list[[1]])))
#' # Stack
#' mat_stack <- raster::stack(mat_stack)
#' 
#' # Average matrices in raster stack
#' avg_stack <- average_by_group(metadata = metadata,
#'                               mat_list = mat_stack,
#'                               matrix_id = "photo_no",
#'                               grouping_var = "rep_id")
#' # Plot
#' raster::plot(avg_stack)
#' 
#' # Compare to invididual pics
#' # Photos 8565 and 8583 were averaged to make T7P1
#' # Photos 8589 and 8613 were averaged to make T7P2
#' 
#' allstack <- raster::stack(mat_stack, avg_stack)
#' rasterVis::levelplot(allstack,
#'                     layout=c(2, 3),
#'                     index.cond=list(c(1, 3, 2, 4, 5, 6)))
#'                     
#' @export
#'
# Define function to return stats for each grouping
average_by_group <- function(metadata,
                             mat_list,
                             matrix_id,
                             grouping_var,
                             round_val = NULL){
    
    # Determine if raster stack or list of matrices
    if(class(mat_list)[1] == "RasterStack"){
        mat_type <- "rasters"
    }else mat_type <- "matrices"
    
    if (requireNamespace("pbapply", quietly = TRUE)) {
        mat_avgs <-
            pbapply::pblapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      # Calculate mean for each group
                                      group_avg <- avg_sub(metadata = metadata, 
                                                           mat_list = mat_list,
                                                           matrix_id = matrix_id,
                                                           grouping_var = grouping_var, 
                                                           grouping_val = x,
                                                           mat_type = mat_type,
                                                           round_val = round_val)
                                      
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
                           group_avg <- avg_sub(metadata = metadata, 
                                                mat_list = mat_list,
                                                matrix_id = matrix_id,
                                                grouping_var = grouping_var, 
                                                grouping_val = x,
                                                mat_type = mat_type)
                           
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
    
    if(mat_type == "rasters"){
        # Drop NAs
        mat_avgs <- mat_avgs[!(is.na(mat_avgs))]
        # Stack
        mat_avgs <- raster::stack(mat_avgs)
    }else{
        # Name
        names(mat_avgs) <- unique(metadata[, grouping_var])
        # Drop NAs
        mat_avgs <- mat_avgs[!(is.na(mat_avgs))]
    }
    
    return(mat_avgs)
}

avg_sub <- function(metadata,
                    mat_list,
                    matrix_id,
                    grouping_var,
                    grouping_val,
                    mat_type,
                    round_val = NULL){
    
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
    
    if(mat_type == "rasters"){
        # Subset
        substack <- mat_list[[inds]]
        # Create mean raster
        avg <- raster::mean(substack)
        # If rounding desired, round here
        if(!(is.null(round_val))) avg <- round(avg, digits = round_val)
        # Name it
        names(avg) <- grouping_val
    }else{
        # Subset
        substack <- mat_list[inds]
        # Create mean matrix
        avg <- apply(simplify2array(substack), 1:2, mean)
        # If rounding desired, round here
        if(!(is.null(round_val))) avg <- round(avg, digits = round_val)
    }
    
    # Return
    return(avg)
}


