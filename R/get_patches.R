#' get_patches
#'
#' Find hot and cold patches in a numeric matrix or raster, and calculate patch statistics.
#' @param img A numeric temperature matrix (such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}) or raster.
#' @param id The image ID (optional). Useful when iterating over
#' numerous images.
#' @param style Style to use when calculating neighbourhood weights using
#'  \code{spdep::}\code{\link[spdep]{nb2listw}}. Defaults to 'C' (globally 
#'  standardised).
#' @param img_proj Spatial projection. Optional, but necessary for geographic
#' data to plot correctly.
#' @param img_extent Spatial extent. Optional, but necessary for geographic
#' data to plot correctly.
#' @param return_vals Which values to return? Any combination of the dataframe
#' (\code{df}), SpatialPolygonsDataFrame of hot and cold patches
#' (\code{patches}) and patch statistics dataframe (\code{pstats}).
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the pixel value (val); the spatial location of the pixel (x and y);
#'  its patch classification (G_bin) into a hot (1), cold (-1) or no patch (0)
#'  according to the Z value (see \code{spdep::}\code{\link[spdep]{localG}});
#'  the unique ID of the patch in which the pixel fell;
#'  the image ID (if applicable); and the original spatial location of the pixel 
#'  (x_orig and y_orig).}
#'  \item{patches}{A SpatialPolygonsDataFrame of hot and cold patches, named
#'  according to \code{id} (if applicable). Hot patches have a value of 1, 
#'  and cold patches a value of -1.}
#'  \item{pstats}{A dataframe with patch statistics for hot patches and cold
#'  patches, respectively. See \code{\link{patch_stats}} for details of all the
#'  statistics returned.}
#' @examples
#'
#' # FLIR temperature matrix ----------------------------------------
#' # Find hot and cold patches
#' flir_results <-
#'     get_patches(img = flir11835$flir_matrix,
#'                 id = flir11835$photo_no)
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
#' # Worldclim2 temperature raster ----------------------------------
#' # Dataset 'sulawesi_temp' represents mean January temperature for 
#' # the island of Sulawesi
#'
#' # Define projection and extent
#' img_proj <- raster::projection(sulawesi_temp)
#' img_extent <- raster::extent(sulawesi_temp)
#'
#' # Find hot and cold patches
#' worldclim_results <-
#'  get_patches(img = sulawesi_temp,
#'              id = "sulawesi",
#'              img_proj = img_proj,
#'              img_extent = img_extent)
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
get_patches <- function(img, 
                        id = NULL, 
                        style = "C",
                        img_proj = NULL,
                        img_extent = NULL,
                        return_vals = c("df","patches","pstats")) {
    
    # Setup --------------------------------------------------------------------
    
    message("Getting patches")
    
    # Get dimensions
    nrows <- nrow(img)
    ncols <- ncol(img)
    
    # Determine whether matrix or raster
    if(is.matrix(img)){
        # Melt to dataframe
        df <- reshape2::melt(img,
                             varnames = c("y", "x"),
                             value.name = "val")
    }else if(class(img)[1] == "RasterLayer"){
        # Coerce to dataframe
        df <- raster::as.data.frame(img, xy = TRUE)
        colnames(df)[3] <- "val"
    }
    
    # Remove NA values
    df <- df[!(is.na(df[, "val"])),]
    
    # Neighbour weights -------------------------------------------------------
    message("\t...calculating neighbourhood weights")
    
    # Identify nearest 8 neighbours
    nr_neigh <- spdep::knearneigh(cbind(df$x, df$y), k = 8)
    nr_neigh <- spdep::knn2nb(nr_neigh)
    
    # Get neighbour weights
    nb_weights <- spdep::nb2listw(nr_neigh, 
                                  style = style, 
                                  zero.policy = FALSE)
    
    # Local Getis-Ord ---------------------------------------------------------
    message("\t...calculating local G statistic")
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
    message("\t...matching pixels to hot and cold patches")
    
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
    
    # Specify coordinates and img_projection (if applicable)
    if(!(is.null(img_proj))){
        raster::extent(patches) <- img_extent
        raster::projection(patches) <- img_proj
    }
    
    # Raster to dissolved polygons
    patches <- suppressMessages(raster::rasterToPolygons(patches, dissolve = TRUE))
    patches <- raster::disaggregate(patches)
    
    # 2. Assign to each temperature cell the ID of the patch that it falls into
    # Matrix to raster to points
    img <- reshape2::acast(df, y ~ x, value.var = "val")
    # Flip
    img <-
        Thermimage::mirror.matrix(Thermimage::rotate180.matrix(img))
    
    # Matrix to raster
    raw <- raster::raster(img,
                          xmn=0, xmx=ncols,
                          ymn=0, ymx=nrows)
    
    # Specify coordinates and img_projection (if applicable)
    if(!(is.null(img_proj))){
        raster::extent(raw) <- img_extent
        raster::projection(raw) <- img_proj
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
    
    if(!(is.null(id))){
        if(is.numeric(id)) id <- paste("X", id, sep = "")
        df$id <- id
        names(patches) <- id
    }
    
    ### 3. Calculate patch stats
    
    message("\t...calculating patch statistics")
    
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
            results <- patch_stats(mat = patch_mat, class = class)
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
                cols <- tolower(gsub(" ", "", gsub("[.]", "", colnames(sumstats))))
                cols[grep("1stqu", cols)] <- "lowerquant"
                cols[grep("3rdqu", cols)] <- "upperquant"
                colnames(sumstats) <- cols
                
            # If no patches in this class, fill manually
            }else{
                sumstats <- data.frame(rbind(rep(NA,6)))
                colnames(sumstats) <- 
                    c("min","lowerquant","median","mean","upperquant","max")
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
    
    # Add image ID if available
    if(!(is.null(id))) pstats[,"id"] <- id
    
    # Return results ------------------------------------------------------------
    if(length(return_vals) == 1){
        return_vals <- eval(parse(text = return_vals))
    }else{
        return_vals <- sapply(return_vals, function(x) eval(parse(text = x)))
    }
    
    return(return_vals)
    
}
