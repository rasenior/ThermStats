#' get_patches
#'
#' Find hot and cold spots in a FLIR thermal image, and calculate their patch statistics.
#' @param flir_matrix A matrix returned from the Thermimage package.
#' @param photo_no The photo number of the FLIR jpeg from which the matrix was derived.
#' @param k Number of neighbours to use when calculating nearest neighbours using spdep::knearneigh
#' @param style Style to use when calculating neighbourhood weights using spdep::nb2listw.
#' @param stats Patch statistics to return from SDMTools::ClassStat.
#' @return List including dataframe of temperature data with pixels matched to patches, patches as a SpatialPolygonsDataFrame, and a dataframe with overall patch statistics.
#' @examples
#' data("FLIR11835")
#' flir_patches(flir_matrix = FLIR1185, photo_no = FLIR_no)
#' @export
#' @importClassesFrom sp SpatialPolygonsDataFrame SpatialPointsDataFrame
flir_patches <- function(flir_matrix, photo_no, k = 8, style = "W",
                         stats = c("n.patches", "total.area",
                                   "obs.edges", "max.edges",
                                   "max.patch.temp", "min.patch.temp")) {

  # Setup ----------------------------------------------------------------------
  cat("\nProcessing photo number:", photo_no, "\n")

  # Matrix needs to be long dataframe for calculating neighbour weights
  flir_df <- reshape2::melt(flir_matrix, varnames = c("y", "x"),
                            value.name = "temp")

  # Neighbour weights -------------------------------------------------------

  # Calculate neighbour weights for K nearest neighbours
  cat("Calculating neighbourhood weights\n")
  flir_nb <-
    spdep::knn2nb(spdep::knearneigh(cbind(flir_df$x, flir_df$y), k = k))
  flir_nb <-
    spdep::nb2listw(neighbours = flir_nb, style = style, zero.policy = FALSE)

  # Local Getis-Ord ---------------------------------------------------------

  cat("Calculating local G statistic\n")
  flir_g <-
    spdep::localG(x = spdep::spNamedVec("temp", flir_df),
                  listw = flir_nb, zero.policy = FALSE, spChk = NULL)

  # Add Z-values to dataframe
  flir_df <- data.frame(flir_df, Z_val = matrix(flir_g))

  rm(flir_nb, flir_g)

  # Define significance categories, using critical value of 3.886 as
  # defined in documentation for sample sizes of > 1000.
  flir_df$G_bin <- ifelse(flir_df$Z_val >= 3.886, 1,
                          ifelse(flir_df$Z_val <= -3.886, -1, 0))

  # Patch statistics --------------------------------------------------------
  cat("Matching pixels to hot and cold spots\n")

  # 1. Create layer with one polygon for each hot/cold patch Dataframe to matrix
  patch_mat <- reshape2::acast(flir_df, y ~ x, value.var = "G_bin")
  # Matrix to raster to polygons
  patches <- raster::raster(patch_mat)
  patches <- raster::rasterToPolygons(patches, dissolve = TRUE)
  patches <- raster::disaggregate(patches)

  # 2. Assign to each temperature cell the ID of the patch that it falls into

  # Matrix to raster to points
  raw <- raster::raster(flir_matrix)
  raw <- raster::rasterToPoints(raw, spatial = TRUE)

  # Return overlay list where each element is the point, and within that the
  # value is the hot/coldspot classification and the row
  # name is the patch ID
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
  flir_df[, c("G_bin", "patchID")] <- patchID

  rm(raw, patchID)

  ### 3. Calculate patch stats

  cat("Calculating patch statistics\n")

  # For each thermal image, want to know the difference between the hottest
  # hot patch and the coldest cold patch, the total area of hot and cold
  # patches, and the AI of hot and cold patches

  # Calculate spatial patch statistics
  patch_stats <- SDMTools::ClassStat(mat = patch_mat, cellsize = 1,
                                     bkgd = 0, latlon = FALSE)

  # Calculate the actual observed number of shared edges for each class
  # First calculate the max edges that could be shared for each class
  patch_stats$n <- floor(sqrt(patch_stats$total.area))
  patch_stats$m <- patch_stats$total.area - (patch_stats$n)^2

  # Function to calculate the maximum number of edges that could be shared
  maxEdges <- function(n, m) {
    if (m == 0) {
      maxEdges <- 2 * n * (n - 1)
    } else if (m <= n) {
      maxEdges <- 2 * n * (n - 1) + (2 * m) - 1
    } else if (m > n) {
      maxEdges <- 2 * n * (n - 1) + (2 * m) - 2
    }

    return(maxEdges)
  }

  patch_stats$max.edges <- mapply(maxEdges, patch_stats$n, patch_stats$m)

  # Observed edges is then the AI multipled by the max number of edges
  patch_stats$obs.edges <-
    (patch_stats$aggregation.index/100) * patch_stats$max.edges

  # Calculate median temperature for each patch
  patch_temp <-
    dplyr::summarise(dplyr::group_by(flir_df[flir_df$G_bin != 0, ],
                                     patchID, G_bin),
                     temp = median(temp))

  # Add max and min patch temp for each patch class
  patch_stats <-
    cbind(patch_stats,
          dplyr::summarise(dplyr::group_by(patch_temp, G_bin),
                           max.patch.temp = max(temp),
                           median.patch.temp = median(temp),
                           min.patch.temp = min(temp))[, 2:4])

  # Reduce to variables of interest
  patch_stats <- patch_stats[, c("class", stats)]

  # Reformat, to ultimately give one row of stats per thermal image

  # If there are no hot spots, need to fill with NAs
  if (!(1 %in% patch_stats$class)) {
    hot_stats <- as.data.frame(t(c(0, rep(NA, length(stats) - 1))))
  } else {
    hot_stats <- patch_stats[patch_stats[, "class"] == 1, -1]
  }

  # If there are no cold spots, need to fill with NAs
  if (!(-1 %in% patch_stats$class)) {
    cold_stats <- as.data.frame(t(c(0, rep(NA, length(stats) - 1))))
  } else {
    cold_stats <- patch_stats[patch_stats[, "class"] == -1, -1]
  }

  colnames(hot_stats) <- paste("hot.", stats, sep = "")
  colnames(cold_stats) <- paste("cold.", stats, sep = "")

  patch_stats <- cbind(photo_no, hot_stats, cold_stats)

  # Return results ------------------------------------------------------------
  return(list(flir_df = flir_df, patches = patches, patch_stats = patch_stats))

}
