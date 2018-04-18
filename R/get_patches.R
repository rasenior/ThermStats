#' get_patches
#'
#' Find hot and cold spots in a FLIR thermal image, and calculate their patch statistics.
#' @param mat A numeric temperature matrix, such as that returned from \code{Thermimage}.
#' @param mat_id The photo number of the FLIR jpeg from which the matrix was derived.
#' @param k Number of neighbours to use when calculating nearest neighbours using \code{spdep::knearneigh}.
#' @param style Style to use when calculating neighbourhood weights using \code{spdep::nb2listw}.
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the original position of the pixel (y and x); its temperature (temp); its
#'  Z value from \code{spdep::localG}; its classification (G_bin) into hot (1) or cold
#'  spots (-1) according to the Z value (see \code{?localG}); the unique ID
#'  of the patch in which the pixel fell; and the photo number.}
#'  \item{patches}{A SpatialPolygonsDataFrame of hot and cold spots. Hot spots
#'  have a value of 1, and cold spots a value of -1.}
#'  \item{patch_stats}{A dataframe with patch statistics for hot spots and cold spots,
#'  respectively.}
#' @examples
#' # Find hot and cold spots
#' results <- get_patches(mat = flir11835$flir_matrix,mat_id = flir11835$mat_id)
#'
#' # Look at the results for individual pixels
#' head(results$df)
#'
#' # Look at the patch statistics for hot and cold spots
#' results$patch_stats
#'
#' # Plot the patches
#' library(sp)
#' plot(results$patches)
#' @export
#' @importClassesFrom sp SpatialPolygonsDataFrame SpatialPointsDataFrame
#'
get_patches <- function(mat, mat_id, k = 8, style = "W",
                        return_vals = c("df","patches","patch_stats")) {

  # Setup ----------------------------------------------------------------------
  message("Processing matrix: ", mat_id)

  # Matrix needs to be long dataframe for calculating neighbour weights
  df <- reshape2::melt(mat,
                       varnames = c("y", "x"),
                       value.name = "var")
  # Remove NA values
  df <- df[!(is.na(df[, "var"])),]

  # Neighbour weights -------------------------------------------------------
  message("Calculating neighbourhood weights")
  # Identify k nearest neighbours
  nr_neigh <- spdep::knearneigh(cbind(df$x, df$y), k = k)
  nr_neigh <- spdep::knn2nb(nr_neigh)
  # Get neighbour weights
  nb_weights <- spdep::nb2listw(nr_neigh, style = style, zero.policy = FALSE)

  # Local Getis-Ord ---------------------------------------------------------
  message("Calculating local G statistic")
  local_g <-
    spdep::localG(x = spdep::spNamedVec("var", df),
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
  message("Matching pixels to hot and cold spots")

  # 1. Create layer with one polygon for each hot/cold patch
  # Dataframe to matrix
  patch_mat <- reshape2::acast(df, y ~ x, value.var = "G_bin")

  # Matrix to raster to polygons
  patches <- raster::raster(patch_mat)
  patches <- raster::rasterToPolygons(patches, dissolve = TRUE)
  patches <- raster::disaggregate(patches)

  # 2. Assign to each temperature cell the ID of the patch that it falls into

  # Matrix to raster to points
  mat <- reshape2::acast(df, y ~ x, value.var = "var")
  raw <- raster::raster(mat)
  raw <- raster::rasterToPoints(raw, spatial = TRUE)

  # Return overlay list where each element is the point, and within that the
  # value is the hot/coldspot classification and the row name is the patch ID
  patchID <- sp::over(raw, patches, returnList = TRUE)

  # Define function to reformat each dataframe within the list
  reform <- function(x) {
    x$patchID <- row.names(x)
    colnames(x) <- c("G_bin", "patchID")
    return(x)
  }

  # Apply function to each list element
  patchID <- lapply(patchID, reform)

  # Bind together
  patchID <- do.call("rbind", patchID)

  # Add to original dataframe
  df[, c("G_bin", "patchID")] <- patchID

  rm(raw, patchID)

  df$mat_id <- mat_id

  ### 3. Calculate patch stats

  message("Calculating patch statistics")

  # Number of pixels in hot/cold spots
  patch_stats <- data.frame(table(patch_mat))
  colnames(patch_stats) <- c("class", "px_no")
  patch_stats <- patch_stats[patch_stats$class != 0,]

  # If there are no patches, create output manually
  if(nrow(patch_stats) == 0){
    patch_stats <- data.frame(mat_id = mat_id,
                              hot_px_no = 0,
                              hot_patch_no = 0,
                              hot_max_edges = 0,
                              hot_obs_edges = 0,
                              hot_non_edges = 0,
                              hot_patch_max_temp = NA,
                              hot_patch_median_temp = NA,
                              hot_patch_min_temp = NA,
                              cold_px_no = 0,
                              cold_patch_no = 0,
                              cold_max_edges = 0,
                              cold_obs_edges = 0,
                              cold_non_edges = 0,
                              cold_patch_max_temp = NA,
                              cold_patch_median_temp = NA,
                              cold_patch_min_temp = NA)
  }else{
    # Number hot/cold spots
    patch_stats[,"patch_no"] <- NA
    patch_stats[patch_stats[,"class"] == -1,"patch_no"] <-
      length(unique(df[df[,"G_bin"]== -1,"patchID"]))
    patch_stats[patch_stats[,"class"] == 1,"patch_no"] <-
      length(unique(df[df[,"G_bin"]== 1,"patchID"]))

    # Max edges
    patch_stats$n <- floor(sqrt(patch_stats$px_no))
    patch_stats$m <- patch_stats$px_no - (patch_stats$n)^2

    max_edges <- function(n, m) {
      if (m == 0) {
        max_edges <- 2 * n * (n - 1)
      } else if (m <= n) {
        max_edges <- 2 * n * (n - 1) + (2 * m) - 1
      } else if (m > n) {
        max_edges <- 2 * n * (n - 1) + (2 * m) - 2
      }
      return(max_edges)
    }

    patch_stats$max_edges <- mapply(max_edges,
                                    patch_stats$n,
                                    patch_stats$m)

    obs_edges <- count_edges(patch_mat, classes = c(-1,1))
    patch_stats <- merge(patch_stats, obs_edges, by = "class")
    patch_stats$non_edges <- patch_stats$max_edges - patch_stats$obs_edges

    # Calculate median temperature for each patch
    patch_temp <-
      dplyr::summarise(dplyr::group_by(df[df$G_bin != 0, ],
                                       patchID, G_bin),
                       temp = median(temp))

    # Add max and min patch temp for each patch class
    patch_stats <- cbind(patch_stats,
                         dplyr::summarise(dplyr::group_by(patch_temp, G_bin),
                                          patch_max_temp = max(temp),
                                          patch_median_temp = median(temp),
                                          patch_min_temp = min(temp))[, 2:4])

    # Reduce to key variables
    patch_stats <- patch_stats[,-(which(names(patch_stats) %in% c("n", "m")))]

    # Reformat, to ultimately give one row of stats per thermal image

    # If there are no hot spots, need to fill with NAs
    if (!(1 %in% patch_stats$class)) {
      hot_stats <- as.data.frame(t(c(rep(0,2), rep(NA, 6))))
    } else {
      hot_stats <- patch_stats[patch_stats[,"class"] == 1, -1]
    }

    # If there are no cold spots, need to fill with NAs
    if (!(-1 %in% patch_stats$class)) {
      cold_stats <- as.data.frame(t(c(rep(0,2), rep(NA, 6))))
    } else {
      cold_stats <-patch_stats[patch_stats[,"class"] == -1, -1]
    }

    colnames(hot_stats) <- paste("hot_", names(patch_stats)[-1], sep = "")
    colnames(cold_stats) <- paste("cold_", names(patch_stats)[-1], sep = "")

    patch_stats <- cbind(mat_id, hot_stats, cold_stats)
  }

  # Return results ------------------------------------------------------------
  if(length(return_vals) == 1){
    return_vals <- eval(parse(text = return_vals))
  }else{
    return_vals <- sapply(return_vals, function(x) eval(parse(text = x)))
  }

  return(return_vals)

}
