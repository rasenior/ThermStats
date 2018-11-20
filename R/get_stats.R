#' get_stats
#'
#' Calculate summary and spatial statistics across a single matrix or raster.
#' @param val_mat A numeric matrix or raster.
#' @param matrix_id The matrix ID (optional). Useful when iterating over numerous matrices.
#' @param get_patches Whether to identify hot and cold spots. Defaults to TRUE.
#' @param k Number of neighbours to use when calculating nearest neighbours
#' using \code{spdep::}\code{\link[spdep]{knearneigh}}.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}.
#' @param mat_proj Spatial projection. Optional, but necessary for geographic
#' data to plot correctly.
#' @param mat_extent Spatial extent. Optional, but necessary for geographic
#' data to plot correctly.
#' @param return_vals Which values to return? Any combination of the dataframe
#' (\code{df}), SpatialPolygonsDataFrame of hot and cold patches
#' (\code{patches}) and patch statistics dataframe (\code{pstats}). Note that
#' \code{pstats} will always be returned -- if this is not desired, use
#' \code{\link{get_patches}} instead.
#' @param pixel_fns The names of the summary statistics to apply.
#' @param ... Use to specify summary statistics that should be calculated across
#' all pixels. Several helper functions are included for use here:
#' \code{\link{perc_5}}, \code{\link{perc_95}},
#' \code{\link{SHDI}}, \code{\link{SIDI}},
#' \code{\link{kurtosis}} and \code{\link{skewness}}.
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the pixel value (val); the original spatial location of the pixel (x and y);
#'  its patch classification (G_bin) into a hot (1), cold (-1) or no patch (0)
#'  according to the Z value (see \code{spdep::}\code{\link[spdep]{localG}});
#'  the unique ID of the patch in which the pixel fell;
#'  and the matrix ID (if applicable).}
#'  \item{patches}{A SpatialPolygonsDataFrame of hot and cold patches. Hot
#'  patches have a value of 1, and cold patches a value of -1.}
#'  \item{pstats}{A dataframe with patch statistics for hot patches and cold
#'  patches, respectively. See \code{\link{patch_stats}} for details of all the
#'  statistics returned.}
#' @examples
#'
#' # FLIR temperature matrix ---------------------------------------------------
#'
#' # Define individual matrix and raster
#' val_mat <- flir11835$flir_matrix
#' val_raster <- raster::raster(val_mat)
#'
#' # Define matrix ID (the photo number in this case)
#' matrix_id <- flir11835$photo_no
#'
#' # Get stats!
#' get_stats(val_mat = val_mat,
#'           matrix_id = matrix_id,
#'           calc_connectivity = TRUE,
#'           conn_threshold = 1.5,
#'           get_patches = TRUE,
#'           k = 8,
#'           style = "W",
#'           mat_proj = NULL,
#'           mat_extent = NULL,
#'           return_vals = "pstats",
#'           sum_stats = c("mean", "min","max"))
#'
#' get_stats(val_mat = val_raster,
#'           matrix_id = matrix_id,
#'           calc_connectivity = TRUE,
#'           conn_threshold = 1.5,
#'           get_patches = TRUE,
#'           k = 8,
#'           style = "W",
#'           mat_proj = NULL,
#'           mat_extent = NULL,
#'           return_vals = "pstats",
#'           sum_stats = c("mean", "min","max"))
#'
#' # Worldclim2 temperature raster ---------------------------------------------
#'
#' # Dataset 'sulawesi_temp' represents mean January temperature for the
#' # island of Sulawesi
#'
#' # Define projection and extent
#' mat_proj <- projection(sulawesi_temp)
#' mat_extent <- extent(sulawesi_temp)
#'
#' # Find hot and cold patches
#' worldclim_results <-
#'  get_stats(val_mat = sulawesi_temp,
#'            matrix_id = "sulawesi",
#'            calc_connectivity = FALSE,
#'            k = 8,
#'            style = "W",
#'            mat_proj = mat_proj,
#'            mat_extent = mat_extent,
#'            return_vals = c("df", "patches", "pstats"),
#'            sum_stats = c("mean", "min","max"))
#'
#' # Plot!
#' df <- worldclim_results$df
#' patches <- worldclim_results$patches
#' plot_patches(df, patches, print_plot = TRUE, save_plot = FALSE)
#' @export

get_stats <- function(val_mat,
                      matrix_id = NULL,
                      calc_connectivity = TRUE,
                      conn_threshold = 1.5,
                      get_patches = TRUE,
                      style = "C",
                      mat_proj = NULL,
                      mat_extent = NULL,
                      return_vals = c("df","patches","pstats"),
                      sum_stats = c("mean", "min","max")){
    
    # If raster, coerce to matrix ------------------------------------------------
    if(class(val_mat)[1] == "RasterLayer"){
        val_mat <- raster::as.matrix(val_mat)
    }
    
    # Record no cols & rows
    ncols <- ncol(val_mat)
    nrows <- nrow(val_mat)
    
    # Pixel statistics -----------------------------------------------------------
    # -> these statistics are calculated across all pixels
    
    if(!(is.null(sum_stats))){
        # Apply pixel-level summary stats
        pixel_stats <-
            lapply(sum_stats, function(x) get(x)(val_mat, na.rm = TRUE))
        
        # Coerce to df
        pixel_stats <- as.data.frame(t(do.call("rbind", pixel_stats)))
        
        # Change column names
        colnames(pixel_stats) <- sum_stats
        
    }
    
    # Connectivity ---------------------------------------------------------------
    # -> calculated across pixels
    if(calc_connectivity){
        pixel_conn <- connectivity(val_mat,
                                   conn_thresh = conn_threshold)
        # Order by row & col
        pixel_conn <-
            pixel_conn[order(pixel_conn["y"],
                             pixel_conn["x"]),]
        
        # Summarise
        conn_summ <-
            data.frame(temp_diff_min = min(pixel_conn$temp_diff),
                       temp_diff_mean = mean(pixel_conn$temp_diff),
                       temp_diff_median = median(pixel_conn$temp_diff),
                       temp_diff_max = max(pixel_conn$temp_diff),
                       cc_min = min(pixel_conn$clim_conn),
                       cc_mean = mean(pixel_conn$clim_conn),
                       cc_median = median(pixel_conn$clim_conn),
                       cc_max = max(pixel_conn$clim_conn))
    }
    
    # Patch statistics -----------------------------------------------------------
    # -> these statistics are calculated for hot and cold patches
    
    # return_vals must include pstats (otherwise just use get_patches)
    if(!("pstats" %in% return_vals)) return_vals <- c(return_vals, "pstats")
    
    if(get_patches){
        all_stats <- get_patches(val_mat = val_mat,
                                 matrix_id = matrix_id,
                                 style = style,
                                 mat_proj = mat_proj,
                                 mat_extent = mat_extent,
                                 return_vals = return_vals)
        
        if("df" %in% return_vals){
            # Rescale coordinates
            all_stats[["df"]]["x_orig"] <- all_stats[["df"]]["x"] * ncols + 0.5
            all_stats[["df"]]["y_orig"] <- all_stats[["df"]]["y"] * nrows + 0.5
            
            # Order by row & col
            all_stats[["df"]] <-
                all_stats[["df"]][order(all_stats[["df"]]["y"],
                                        all_stats[["df"]]["x"]),]
        }
        
        # Return results -------------------------------------------------------------
        
        # If length one then only includes pstats
        if(length(return_vals) == 1){
            
            # If calculating connectivity, add results to patch stats & pixel stats
            if(calc_connectivity){
                all_stats <- cbind(pixel_stats, conn_summ, all_stats)
                
                if("df" %in% return_vals){
                    # Bind connectivity results to df
                    all_stats[["df"]] <-
                        cbind(all_stats[["df"]],
                              pixel_conn[!(names(pixel_conn) %in% c("x","y","temp"))])
                    
                }
            # Just add pixel stats results to patch stats
            } else{
                all_stats <- cbind(pixel_stats, all_stats)
            }
        }else{
            
            # If calculating connectivity, add results to patch stats & temp df
            if(calc_connectivity){
                all_stats[["pstats"]] <- cbind(pixel_stats,
                                               conn_summ,
                                               all_stats[["pstats"]])
                if("df" %in% return_vals){
                    # Bind connectivity results to df
                    all_stats[["df"]] <-
                        cbind(all_stats[["df"]],
                              pixel_conn[!(names(pixel_conn) %in% c("x","y","temp"))])
                }
            # Just add pixel stats results to patch stats
            }else{
                all_stats[["pstats"]] <- cbind(pixel_stats, all_stats[["pstats"]])
            }
        }
        
        return(all_stats)
    }else{
        return(pixel_stats)
    }
    
}
