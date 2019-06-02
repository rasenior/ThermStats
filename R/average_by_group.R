#' average_by_group
#'
#' Average multiple matrices or rasters within groups.
#' @param metadata A dataframe denoting the grouping of different images.
#' @param img_list List or stack of numeric temperature matrices or rasters.
#' @param id Name of the metadata variable that identifies unique
#' images. Should match element names in the image list.
#' @param grouping_var The name of the metadata variable that denotes the
#' grouping of images.
#' @param round_val Number of decimals to round to (optional).
#' @return 
#' If \code{img_list} is supplied as a list of matrices, returns
#' a new list of matrices with one element for each unique value of the grouping
#' variable. 
#' 
#' If \code{img_list} is supplied as a raster stack, returns a new raster
#' stack with one raster for each unique value of the grouping variable. 
#' 
#' Grouping variables with no matching images are dropped.
#' @examples
#' # Load raw data
#' raw_dat <- flir_raw$raw_dat
#' camera_params <- flir_raw$camera_params
#' metadata <- flir_metadata
#'
#' # Batch convert
#' img_list <- batch_convert(raw_dat, write_results = FALSE)
#' 
#' # With list of matrices -----------------------------------------------------
#' 
#' # Average matrices in list
#' avg_list <- average_by_group(metadata = metadata,
#'                              img_list = img_list,
#'                              id = "photo_no",
#'                              grouping_var = "rep_id")
#'                              
#' # With raster stack ---------------------------------------------------------
#' 
#' # Coerce matrix list to raster stack
#' mat_stack <- stack_imgs(img_list)
#' 
#' # Average in raster stack
#' avg_stack <- average_by_group(metadata = metadata,
#'                               img_list = mat_stack,
#'                               id = "photo_no",
#'                               grouping_var = "rep_id")
#' # Plot
#' raster::plot(avg_stack)
#' 
#' # Compare to invididual pics
#' # Photos 8565 and 8583 were averaged to make T7P1
#' # Photos 8589 and 8613 were averaged to make T7P2
#' 
#' allstack <- raster::stack(mat_stack, avg_stack)
#' if (requireNamespace("rasterVis", quietly = TRUE)){
#'     rasterVis::levelplot(allstack,
#'                         layout=c(2, 3),
#'                         index.cond=list(c(1, 3, 2, 4, 5, 6)))
#' }
#'                     
#' @export
#'
# Define function to return stats for each grouping
average_by_group <- function(metadata,
                             img_list,
                             id,
                             grouping_var,
                             round_val = NULL){
    
    # Test that id variable in metadata
    if (!(id %in% names(metadata))) {
        stop("Given id variable is missing from metadata")
    }
    # Test that grouping variable in metadata
    if (!(grouping_var %in% names(metadata))) {
        stop("Given grouping variable is missing from metadata")
    }
    
    # Determine if raster stack or list of matrices
    if(class(img_list)[1] == "RasterStack"){
        mat_type <- "rasters"
    }else mat_type <- "matrices"
    
    if (requireNamespace("pbapply", quietly = TRUE)) {
        mat_avgs <-
            pbapply::pblapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      # Calculate mean for each group
                                      group_avg <- avg_sub(metadata = metadata, 
                                                           img_list = img_list,
                                                           id = id,
                                                           grouping_var = grouping_var, 
                                                           grouping_val = x,
                                                           mat_type = mat_type,
                                                           round_val = round_val)
                                      
                                      # Return
                                      return(group_avg)
                                      
                                  },
                                  error = function(err) {
                                      
                                      # error handler picks up where error was generated
                                      message(paste("\nError:  ",err))
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
                                                img_list = img_list,
                                                id = id,
                                                grouping_var = grouping_var, 
                                                grouping_val = x,
                                                mat_type = mat_type)
                           
                           # Return
                           return(group_avg)
                           
                       },
                       error = function(err) {
                           
                           # error handler picks up where error was generated
                           message(paste("\nError:  ",err))
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
                    img_list,
                    id,
                    grouping_var,
                    grouping_val,
                    mat_type,
                    round_val = NULL){
    
    # Define IDs for the group
    ids <- unique(metadata[metadata[,grouping_var] == grouping_val, id])
    
    # If raster stack need to paste 'X' onto numeric ids
    if(mat_type == "rasters" & is.numeric(ids)) ids <- paste("X", ids, sep = "")
    
    # Define the indices of these images in the matrix list
    inds <- which(names(img_list) %in% ids)
    
    # If there are no matches, return NA
    if(length(inds) == 0){
        # Return
        return(NA) 
    }
    
    if(mat_type == "rasters"){
        # Subset
        substack <- img_list[[inds]]
        # Create mean raster
        avg <- raster::mean(substack)
        # If rounding desired, round here
        if(!(is.null(round_val))) avg <- round(avg, digits = round_val)
        # Name it
        names(avg) <- grouping_val
    }else{
        # Subset
        substack <- img_list[inds]
        # Create mean matrix
        avg <- apply(simplify2array(substack), 1:2, mean)
        # If rounding desired, round here
        if(!(is.null(round_val))) avg <- round(avg, digits = round_val)
    }
    
    # Return
    return(avg)
}


