#' get_patches
#'
#' Find hot and cold patches in a numeric matrix or raster, and calculate patch statistics.
#' @param val_mat A numeric matrix (such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}) or a raster.
#' @param matrix_id The matrix ID (optional). Useful when iterating over
#' numerous matrices.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}. Defaults to 'C' (globally 
#'  standardised).
#' @param mat_proj Spatial projection. Optional, but necessary for geographic
#' data to plot correctly.
#' @param mat_extent Spatial extent. Optional, but necessary for geographic
#' data to plot correctly.
#' @param return_vals Which values to return? Any combination of the dataframe
#' (\code{df}), SpatialPolygonsDataFrame of hot and cold patches
#' (\code{patches}) and patch statistics dataframe (\code{pstats}).
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the pixel value (val); the original spatial location of the pixel (x and y);
#'  patch classification (G_bin) into hot (1), cold (-1) or no patch (0)
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
#' # FLIR temperature matrix ---------------------------------------------
#' # Find hot and cold patches
#' flir_results <-
#'     get_patches(val_mat = flir11835$flir_matrix,
#'     matrix_id = flir11835$photo_no)
#'
#' # Look at the results for individual pixels
#' head(flir_results$df)
#'
#' # Look at the patch statistics for hot and cold patches
#' flir_results$pstats
#'
#' # Plot the patches
#' sp::plot(flir_results$patches)
#'
#' # Plot using ThermStats::plot_patches
#' plot_patches(df = flir_results$df,
#'              patches = flir_results$patches,
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#'
#' # Worldclim2 temperature raster ---------------------------------------
#' # Dataset 'sulawesi_temp' represents mean January temperature for the
#' # island of Sulawesi
#'
#' # Define projection and extent
#' mat_proj <- raster::projection(sulawesi_temp)
#' mat_extent <- raster::extent(sulawesi_temp)
#'
#' # Find hot and cold patches
#' worldclim_results <-
#'  get_patches(val_mat = sulawesi_temp,
#'              matrix_id = "sulawesi",
#'              mat_proj = mat_proj,
#'              mat_extent = mat_extent)
#'
#' # Look at the results for individual pixels
#' head(worldclim_results$df)
#'
#' # Look at the patch statistics for hot and cold patches
#' worldclim_results$pstats
#'
#' # Plot the patches
#' sp::plot(worldclim_results$patches)
#'
#' # Plot using ThermStats::plot_patches
#' plot_patches(df = worldclim_results$df,
#'              patches = worldclim_results$patches,
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#'
#' @importClassesFrom sp SpatialPolygonsDataFrame SpatialPointsDataFrame
#' @export
#'
get_patches <- function(val_mat, 
                        matrix_id = NULL, 
                        style = "C",
                        mat_proj = NULL,
                        mat_extent = NULL,
                        return_vals = c("df","patches","pstats")) {
    
    # Setup ----------------------------------------------------------------------
    if(!(is.null(matrix_id))){
        message("\nProcessing: ", matrix_id)
    }
    
    # Get matrix dimensions
    nrows <- nrow(val_mat)
    ncols <- ncol(val_mat)
    
    # Determine whether the matrix is a true matrix or a raster
    if(is.matrix(val_mat)){
        # Melt to dataframe
        df <- reshape2::melt(val_mat,
                             varnames = c("y", "x"),
                             value.name = "val")
    }else if(class(val_mat)[1] == "RasterLayer"){
        # Coerce to dataframe
        df <- raster::as.data.frame(val_mat, xy = TRUE)
        colnames(df)[3] <- "val"
    }
    
    # Remove NA values
    df <- df[!(is.na(df[, "val"])),]
    
    # Neighbour weights -------------------------------------------------------
    message("\nCalculating neighbourhood weights")
    
    # Identify nearest 8 neighbours
    nr_neigh <- spdep::knearneigh(cbind(df$x, df$y), k = 8)
    nr_neigh <- spdep::knn2nb(nr_neigh)
    
    # Get neighbour weights
    nb_weights <- spdep::nb2listw(nr_neigh, 
                                  style = style, 
                                  zero.policy = FALSE)
    
    # Local Getis-Ord ---------------------------------------------------------
    message("Calculating local G statistic")
    local_g <-
        spdep::localG(x = spdep::spNamedVec("val", df),
                      listw = nb_weights,
                      zero.policy = FALSE,
                      spChk = NULL)
    
    # Add Z-values to dataframe
    df <- data.frame(df, Z_val = matrix(local_g))
    
    rm(nr_neigh, nb_weights, local_g)
    
    # Define significance categories, using critical values defined in  Getis and
    # Ord (1996 )
    get_critical_Z <- function(n){
        if(n >= 1 & n < 10){
            critical_z <- 1.6450
        }else if(n >= 10 & n < 30){
            critical_z <- 2.5683
        }else if(n >= 30 & n < 50){
            critical_z <- 2.9291
        }else if(n >= 50 & n < 100){
            critical_z <- 3.0833
        }else if(n >= 100 & n < 1000){
            critical_z <- 3.2889
        }else if(n > 1000){
            critical_z <- 3.8855
        }else{
            critical_z <- NA
        }
    }
    critical_Z <- get_critical_Z(nrow(df))
    
    df$G_bin <- ifelse(df$Z_val >= critical_Z, 1,
                       ifelse(df$Z_val <= -critical_Z, -1, 0))
    
    # Patch statistics --------------------------------------------------------
    message("Matching pixels to hot and cold patches")
    
    # 1. Create layer with one polygon for each hot/cold patch
    # Dataframe to matrix
    patch_mat <- reshape2::acast(df, y ~ x, value.var = "G_bin")
    
    # Flip
    patch_mat <-
        Thermimage::mirror.matrix(Thermimage::rotate180.matrix(patch_mat))
    
    # Rasterise matrix
    # patches <- raster::raster(patch_mat)
    patches <-
        raster::raster(patch_mat,
                       xmn=0, xmx=ncols,
                       ymn=0, ymx=nrows)
    
    # Specify coordinates and mat_projection (if applicable)
    if(!(is.null(mat_proj))){
        raster::extent(patches) <- mat_extent
        raster::projection(patches) <- mat_proj
    }
    
    # Raster to dissolved polygons
    patches <- raster::rasterToPolygons(patches, dissolve = TRUE)
    patches <- raster::disaggregate(patches)
    
    # 2. Assign to each temperature cell the ID of the patch that it falls into
    # Matrix to raster to points
    val_mat <- reshape2::acast(df, y ~ x, value.var = "val")
    # Flip
    val_mat <-
        Thermimage::mirror.matrix(Thermimage::rotate180.matrix(val_mat))
    
    # Matrix to raster
    raw <- raster::raster(val_mat,
                          xmn=0, xmx=ncols,
                          ymn=0, ymx=nrows)
    
    # Specify coordinates and mat_projection (if applicable)
    if(!(is.null(mat_proj))){
        raster::extent(raw) <- mat_extent
        raster::projection(raw) <- mat_proj
    }
    
    # Raster to points
    raw <- raster::rasterToPoints(raw, spatial = TRUE)
    
    # Return overlay list where each element is the point, and within that the
    # value is the hot/coldspot classification and the row name is the patch ID
    n_features <- length(raw)
    
    # Define function to overlay in subsets if too many features
    over_sub <- function(min_i, max_i){
        subdat <- raw[(min_i + 1):max_i,]
        patchID <- sp::over(subdat, patches, returnList = TRUE)
        return(patchID)
    }
    
    # Define function to reformat each dataframe within the list
    reform <- function(x) {
        x$patchID <- row.names(x)
        colnames(x) <- c("G_bin", "patchID")
        return(x)
    }
    
    if(n_features > 10^5){
        lims <- seq(0, n_features, 10^5)
        lims[lims == tail(lims, n = 1)] <- n_features
        
        # Overlay by subset
        patchID <-
            lapply(1:(length(lims) - 1), function(i){
                over_sub(lims[i], lims[i + 1])
            })
        
        patchID <-
            lapply(patchID, function(x){
                # Reform each element (pixel) into a dataframe
                df <-
                    lapply(x, function(y){
                        reform(y)
                    })
                # Bind pixel dataframes within subsets
                df <- do.call("rbind", df)
                return(df)
            })
        
        # Bind all subsets
        patchID <- do.call("rbind", patchID)
        
    }else{
        patchID <- sp::over(raw, patches, returnList = TRUE)
        # Reform each element (pixel) into a dataframe
        patchID <- lapply(patchID, reform)
        
        # Bind pixel dataframes
        patchID <- do.call("rbind", patchID)
    }
    
    # Recreate dataframe
    df <- data.frame(raw)[-4]
    colnames(df)[which(names(df) == "layer")] <- "val"
    df[, c("G_bin", "patchID")] <- patchID
    
    rm(raw, patchID)
    
    if(!(is.null(matrix_id))){
        df$matrix_id <- matrix_id
    }
    
    ### 3. Calculate patch stats
    
    message("Calculating patch statistics")
    
    # Calculate median for each patch --------------------------------------------
    if (requireNamespace("dplyr", quietly = TRUE)) {
        patch_val <-
            dplyr::summarise(dplyr::group_by(df[df$G_bin != 0, ],
                                             patchID, G_bin),
                             val = median(val, na.rm = TRUE))
        patch_val <- as.data.frame(patch_val)
    }else{
        patchIDs <- unique(df[df[,"G_bin"] != 0, "patchID"])
        patch_val <-
            lapply(patchIDs, function(x){
                patchID <- x
                G_bin <- df[df[,"patchID"] == x,"G_bin"][1]
                val <- median(df[df[,"patchID"] == x,"val"], na.rm = TRUE)
                return(data.frame(patchID = patchID,
                                  G_bin = G_bin,
                                  val = val))
            })
        patch_val <- do.call("rbind", patch_val)
    }
    
    # Calculate stats for each class --------------------------------------------
    pstats <-
        lapply(c(1,-1), function(class){
            # Spatial stats
            results <- patch_stats(val_mat = patch_mat, class = class)
            # Patch number
            results[, "abundance"] <-
                length(unique(df[df[,"G_bin"] == class, "patchID"]))
            # Patch density
            results[, "density"] <-
                results[, "abundance"] / results[, "total_area"]
            
            # Summary stats across patches (based on median values)
            if(length(patch_val[patch_val[,"G_bin"] == class, "val"]) > 0){
                sumstats <-
                    t(as.matrix(summary(patch_val[patch_val[,"G_bin"] == class, "val"])))
                colnames(sumstats) <-
                    tolower(gsub(" ", "", gsub("[.]", "", colnames(sumstats))))
                # If no patches in this class, fill manually
            }else{
                sumstats <- data.frame(rbind(rep(NA,6)))
                colnames(sumstats) <- 
                    c("min","1stqu","median","mean","3rdqu","max")
            }
            
            results <- cbind(results, sumstats)
            
            # Remove class column
            results <- results[-1]
            
            # Add patch class prefix to column names
            prefix <- ifelse(class == 1, "hot", "cold")
            colnames(results) <- paste(prefix, colnames(results), sep = "_")
            
            # Return
            return(results)
        })
    
    # Bind class results together
    pstats <- do.call("cbind", pstats)
    
    # Add matrix ID if available
    if(!(is.null(matrix_id))) pstats[,"matrix_id"] <- matrix_id
    
    # Return results ------------------------------------------------------------
    if(length(return_vals) == 1){
        return_vals <- eval(parse(text = return_vals))
    }else{
        return_vals <- sapply(return_vals, function(x) eval(parse(text = x)))
    }
    
    return(return_vals)
    
}
