#' stats_by_group
#'
#' Calculate summary and spatial statistics across multiple images within groups.
#' @param metadata A dataframe denoting the grouping of different images.
#' @param img_list List or stack of numeric temperature matrices or rasters.
#' @param idvar Name of the metadata variable that identifies unique
#' images. Should match element names in the image list.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}.
#' @param grouping_var The name of the metadata variable that denotes the
#' grouping of images.
#' @param round_val Value to round to. Use 1 for no rounding.
#' @param sum_stats Summary statistics that should be calculated across
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
#' img_list <- batch_convert(raw_dat, write_results = FALSE)
#'
#' # Calculate patch and pixel stats! ------------------------------------------
#'
#' # Pixel stats = mean, max and min
#' patch_stats_1 <-
#'     stats_by_group(metadata = metadata,
#'                    img_list = img_list,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("mean", "max", "min"))
#'
#' # Pixel stats = kurtosis and sknewness
#' patch_stats_2 <-
#'     stats_by_group(metadata = metadata,
#'                    img_list = img_list,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("kurtosis", "skewness"))
#'
#' # Pixel stats = 5th and 95th percentiles
#' patch_stats_3 <-
#'     stats_by_group(metadata = metadata,
#'                    img_list = img_list,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("perc_5", "perc_95"))
#'
#' # Pixel stats = Shannon and Simpson Diversity Indices
#' patch_stats_4 <-
#'     stats_by_group(metadata = metadata,
#'                    img_list = img_list,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("SHDI", "SIDI"))
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(metadata,
                           img_list,
                           idvar,
                           grouping_var = NULL,
                           calc_connectivity = FALSE,
                           conn_threshold = 1.5,
                           patches = TRUE,
                           style = "C",
                           return_vals = c("df", "patches", "pstats"),
                           round_val = NULL,
                           sum_stats = c("mean", "max", "min")){
    
    # Determine if raster stack or list of matrices
    if(class(img_list)[1] == "RasterStack"){
        list_type <- "rasterstack"
    }else list_type <- "list"
    
    # If grouping variable is NULL, assume there is only one image per group
    # and therefore the grouping variable is the same as the ID variable
    if(is.null(grouping_var)) grouping_var <- idvar
    
    # return_vals must include pstats
    if(!("pstats" %in% return_vals)) return_vals <- c(return_vals, "pstats")
    
    # Only return pstats if not getting patches
    if(!(patches)) return_vals <- "pstats"
    
    if (requireNamespace("pbapply", quietly = TRUE)) {
        temp_stats <-
            pbapply::pblapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      message("\nProcessing: ", x)
                                      
                                      sub_list <- create_subset(metadata = metadata,
                                                                img_list = img_list,
                                                                id = idvar,
                                                                grouping_var = grouping_var,
                                                                grouping_val = x,
                                                                list_type = list_type,
                                                                round_val = round_val)
                                      
                                      if(length(sub_list) == 0) return(NA)
                                      
                                      n_img <- sub_list[["n_img"]]
                                      sub_list <- sub_list[["sub_list"]]
                                      
                                      # Get stats
                                      result <-
                                          get_stats(img = sub_list,
                                                    id = x,
                                                    calc_connectivity = calc_connectivity,
                                                    conn_threshold = conn_threshold,
                                                    patches = patches,
                                                    style = style,
                                                    img_proj = NULL,
                                                    img_extent = NULL,
                                                    return_vals = return_vals,
                                                    sum_stats = sum_stats
                                          )
                                      
                                      # Add number of images
                                      if(length(return_vals) > 1){
                                          result$pstats <- 
                                              cbind(result$pstats, n_img)
                                      }else result <- cbind(result, n_img)
                                      # Return
                                      return(result)
                                      
                                  },
                                  error = function(err) {
                                      
                                      # error handler picks up where error was generated
                                      message(paste("\nError:  ",err))
                                      return(NA)
                                      
                                  })
                              })
    }else{
        temp_stats <-
            lapply(unique(metadata[, grouping_var]),
                              function(x){
                                  tryCatch({
                                      message("\nProcessing: ", x)
                                      
                                      sub_list <- create_subset(metadata = metadata,
                                                                img_list = img_list,
                                                                id = idvar,
                                                                grouping_var = grouping_var,
                                                                grouping_val = x,
                                                                list_type = list_type,
                                                                round_val = round_val)
                                      
                                      if(length(sub_list) == 0) return(NA)
                                      
                                      n_img <- sub_list[["n_img"]]
                                      sub_list <- sub_list[["sub_list"]]
                                      
                                      # Get stats
                                      result <-
                                          get_stats(img = sub_list,
                                                    id = x,
                                                    calc_connectivity = calc_connectivity,
                                                    conn_threshold = conn_threshold,
                                                    patches = patches,
                                                    style = style,
                                                    img_proj = NULL,
                                                    img_extent = NULL,
                                                    return_vals = return_vals,
                                                    sum_stats = sum_stats
                                          )
                                      
                                      # Add number of images
                                      if(length(return_vals) > 1){
                                          result$pstats <- 
                                              cbind(result$pstats, n_img)
                                      }else result <- cbind(result, n_img)
                                      # Return
                                      return(result)
                                      
                                  },
                                  error = function(err) {
                                      
                                      # error handler picks up where error was generated
                                      message(paste("\nError:  ",err))
                                      return(NA)
                                      
                                  })
                              })
    }
    
    # Drop NAs
    temp_stats <- temp_stats[!(is.na(temp_stats))]
    
    if(length(return_vals) > 1){
    # Bind pstats
    pstats <- 
        do.call("rbind",
            lapply(temp_stats,function(x) rbind(x[["pstats"]])))
    }else{
        pstats <- do.call("rbind", temp_stats)
    }
    
    # Bind df
    if("df" %in% return_vals){
        df <- 
            do.call("rbind",
                    lapply(temp_stats,function(x) rbind(x[["df"]])))
    }
    
    # Bind patches
    if("patches" %in% return_vals){
        patches <- lapply(temp_stats,function(x) rbind(x[["patches"]]))
    }
    
    # Bind everything together
    results <- lapply(return_vals, function(x) eval(parse(text = x)))
    names(results) <- return_vals
    
    # Return
    return(results)
}


create_subset <- function(metadata,
                          img_list,
                          id,
                          grouping_var,
                          grouping_val,
                          list_type,
                          round_val){
    
    # Define matrix IDs for the group
    ids <- unique(metadata[metadata[,grouping_var] == grouping_val, id])
    
    # If raster stack need to paste 'X' onto numeric ids
    if(list_type == "rasterstack" & is.numeric(ids)) ids <- paste("X", ids, sep = "")
    
    # Define the indices of these images in the list
    inds <- which(names(img_list) %in% ids)
    
    # If there are no matches, return empty
    if(length(inds) == 0){
        # Return
        return(NULL) 
    }
    
    # Define the number of images in this group
    n_img <- length(ids)
    
    if(list_type == "rasterstack"){
        # Subset raster stack by the desired matrix IDs
        sub_list <- img_list[[inds]]
        # Unstack
        if(class(sub_list) == "RasterStack"){
            raster_list <- unstack(sub_list)
        }else raster_list <- list(sub_list)
        
    }else{
        # Subset list by the desired matrix IDs
        sub_list <- img_list[inds]
        # Coerce any raster layers to matrices
        raster_list <-
            sub_list[which(sapply(sub_list, function(x) class(x) == "RasterLayer"))]
    }
    
    # Coerce rasters to matrices
    if(length(raster_list) > 0){
        raster_list <- lapply(raster_list, raster::as.matrix)
        
        if(list_type == "rasterstack"){
            # If stack then raster list replaces everything
            sub_list <- raster_list
        }else{
            # Replace only the elements that are rasters
            sub_list[which(sapply(sub_list, function(x) class(x) == "RasterLayer"))] <-
                raster_list
        }
    }
    
    # Round values to desired precision
    if(!is.null(round_val)){
        sub_list<-
            lapply(sub_list, function(x){
                round_val * round(x / round_val)
            })
    }
    
    # Pad with NAs (because they are not actually adjacent in space)
    if(n_img > 1){
        sub_list <-
            lapply(sub_list, function(x){
                cbind(x, NA)
            })
    }
    
    # Bind together
    sub_list <- do.call("cbind", sub_list)
    
    return(list(sub_list = sub_list, n_img = n_img))
}

