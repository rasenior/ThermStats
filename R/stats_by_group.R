#' stats_by_group
#'
#' Calculate summary and spatial statistics across multiple images within groups.
#' @param img_list List or stack of numeric temperature matrices or rasters.
#' @param metadata A dataframe denoting the grouping of different images. 
#' Defaults to NULL as this is not required when \code{img_list} provided as a
#' raster stack (where each raster layer assumed to represent each 'group').
#' @param idvar Name of the metadata variable that identifies unique
#' images. Should match element names in the image list. Defaults to NULL as 
#' this is not required when \code{img_list} provided as a raster stack (where 
#' \code{idvar} is assumed to be the names of the raster layers).
#' @param grouping_var The name of the metadata variable that denotes the
#' grouping of images. Defaults to NULL, where it is assumed to equal \code{idvar} 
#' (each group is assumed to have only one image).
#' @param round_val Value to round to. Defaults to NULL.
#' @param calc_connectivity Whether or not to calculate thermal connectivity
#' across pixels (slow for large rasters). Defaults to FALSE.
#' @param conn_threshold Climate threshold to use for calculation of thermal
#' connectivity (i.e. the amount of change that organisms would be seeking
#' to avoid). See \code{ThermStats::}\code{\link{connectivity}}. 
#' Defaults to 1.5°C.
#' @param patches Whether to identify hot and cold spots. Defaults to TRUE.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}. Defaults to 'C' (globally 
#'  standardised).
#' @param img_proj Spatial projection. Optional, but necessary for geographic
#' data to plot correctly.
#' @param img_extent Spatial extent. Optional, but necessary for geographic
#' data to plot correctly.
#' @param sum_stats Summary statistics that should be calculated across
#' all pixels. Several helper functions are included for use here:
#' \code{\link{perc_5}}, \code{\link{perc_95}},
#' \code{\link{SHDI}}, \code{\link{SIDI}},
#' \code{\link{kurtosis}} and \code{\link{skewness}}.
#' @param return_vals Which values to return? Any combination of the dataframe
#' (\code{df}), SpatialPolygonsDataFrame of hot and cold patches
#' (\code{patches}) and patch statistics dataframe (\code{pstats}), although
#' \code{pstats} will always be returned -- if this is not desired, use
#' \code{\link{get_patches}} instead.
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the pixel value (val); the spatial location of the pixel (x and y);
#'  its patch classification (G_bin) into a hot (1), cold (-1) or no patch (0)
#'  according to the Z value (see \code{spdep::}\code{\link[spdep]{localG}});
#'  the unique ID of the patch in which the pixel fell;
#'  the image ID (if applicable); and the original spatial location of the pixel 
#'  (x_orig and y_orig). 
#'  
#'  If calculating thermal connectivity, \code{df} will also contain: the unique
#'  ID of the coldest destination pixel that can be reached by traversing a 
#'  gradient of hotter to cooler pixels (dest_pixel); the pixel value of the
#'  destination pixel (dest_val); the unique IDs of pixels traversed from origin
#'  to destination (inter_pixel); the temperature difference between the origin
#'  and destination pixel (diff_potential); and the thermal connectivity 
#'  (therm_conn), which is \code{diff_potential} minus \code{conn_threshold}.}
#'  \item{patches}{A list of SpatialPolygonsDataFrames of hot and cold patches, 
#'  named according to \code{grouping_var}. Hot patches have a value of 1, and 
#'  cold patches a value of -1.}
#'  \item{pstats}{A dataframe with patch statistics for hot patches and cold
#'  patches, respectively. See \code{\link{patch_stats}} for details of all the
#'  statistics returned. 
#'  
#'  If calculating thermal connectivity, there will also be statistics for
#'  the minimum, mean, median and maximum temperature difference 
#'  (\code{diff_potential}) and thermal connectivity (\code{conn_threshold}).}
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
#'     stats_by_group(img_list = img_list,
#'                    metadata = metadata,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("mean", "max", "min"))
#'
#' # Pixel stats = kurtosis and sknewness
#' patch_stats_2 <-
#'     stats_by_group(img_list = img_list,
#'                    metadata = metadata,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("kurtosis", "skewness"))
#'
#' # Pixel stats = 5th and 95th percentiles
#' patch_stats_3 <-
#'     stats_by_group(img_list = img_list,
#'                    metadata = metadata,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("perc_5", "perc_95"))
#'
#' # Pixel stats = Shannon and Simpson Diversity Indices
#' patch_stats_4 <-
#'     stats_by_group(img_list = img_list,
#'                    metadata = metadata,
#'                    idvar = "photo_no",
#'                    style = "C",
#'                    grouping_var = "rep_id",
#'                    round_val = 0.5,
#'                    sum_stats = c("SHDI", "SIDI"))
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(img_list,
                           metadata = NULL,
                           idvar = NULL,
                           grouping_var = NULL,
                           round_val = NULL,
                           calc_connectivity = FALSE,
                           conn_threshold = 1.5,
                           patches = TRUE,
                           style = "C",
                           img_proj = NULL,
                           img_extent = NULL,
                           sum_stats = c("mean", "max", "min"),
                           return_vals = c("df", "patches", "pstats")){
    
    # Determine if raster stack or list of matrices
    if(class(img_list)[1] == "RasterStack"){
        list_type <- "rasterstack"
        
        # May not be metadata if it's a raster stack
        if(is.null(metadata) & is.null(idvar)){
            metadata <- data.frame(idvar = names(img_list))
            idvar <- "idvar"
        }
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
                                                    img_proj = img_proj,
                                                    img_extent = img_extent,
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
                                                    img_proj = img_proj,
                                                    img_extent = img_extent,
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

