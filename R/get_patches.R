#' get_patches
#'
#' Find hot and cold spots in a numeric matrix, and calculate their patch
#' statistics.
#' @param val_mat A numeric temperature matrix, such as that returned from
#' \code{Thermimage}.
#' @param matrix_id The matrix ID (optional). Useful when iterating over
#' numerous matrices.
#' @param k Number of neighbours to use when calculating nearest neighbours
#' using \code{spdep::knearneigh}.
#' @param style Style to use when calculating neighbourhood weights using
#' \code{spdep::nb2listw}.
#' @param mat_proj Spatial projection (optional).
#' @param return_vals Which values to return? Any combination of the dataframe
#' ("df"), SpatialPolygonsDataFrame of hot and cold spots ("patches") and patch
#' statistics dataframe ("pstats").
#' @return A list containing:
#'  \item{df}{A dataframe with one row for each pixel, and variables denoting:
#'  the original position of the pixel (y and x); its temperature (temp); its
#'  Z value from \code{spdep::localG}; its classification (G_bin) into hot (1)
#'  or cold spots (-1) according to the Z value (see \code{?localG}); the unique
#'  ID of the patch in which the pixel fell; and the photo number.}
#'  \item{patches}{A SpatialPolygonsDataFrame of hot and cold spots. Hot spots
#'  have a value of 1, and cold spots a value of -1.}
#'  \item{pstats}{A dataframe with patch statistics for hot spots and cold
#'  spots, respectively.}
#' @examples
#' # Find hot and cold spots
#' results <-
#' get_patches(val_mat = flir11835$flir_matrix,
#' matrix_id = flir11835$photo_no)
#'
#' # Look at the results for individual pixels
#' head(results$df)
#'
#' # Look at the patch statistics for hot and cold spots
#' results$pstats
#'
#' # Plot the patches
#' library(sp)
#' plot(results$patches)
#' @export
#' @importClassesFrom sp SpatialPolygonsDataFrame SpatialPointsDataFrame
#'
get_patches <- function(val_mat, matrix_id = NULL, k = 8, style = "W",
                        mat_proj = NULL,
                        coords = NULL,
                        return_vals = c("df","patches","pstats")) {

  # Setup ----------------------------------------------------------------------
  if(!(is.null(matrix_id))){
    message("\nProcessing matrix: ", matrix_id)
  }

  # Determine whether the matrix is a true matrix or a raster
  if(is.matrix(val_mat)){
    df <- reshape2::melt(val_mat,
                              varnames = c("y", "x"),
                              value.name = "val")
  }else if(class(val_mat)[1] == "RasterLayer"){
    df <- as.data.frame(val_mat, xy = TRUE)
    colnames(df)[3] <- "val"
  }

  # Matrix needs to be long dataframe for calculating neighbour weights
  # Remove NA values
  df <- df[!(is.na(df[, "val"])),]

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
  message("Matching pixels to hot and cold spots")

  # 1. Create layer with one polygon for each hot/cold patch
  # Dataframe to matrix
  patch_mat <- reshape2::acast(df, y ~ x, value.var = "G_bin")

  # Flip
  patch_mat <-
    Thermimage::mirror.matrix(Thermimage::rotate180.matrix(patch_mat))

  # Matrix to raster
  patches <- raster::raster(patch_mat)

  # Specify coordinates and mat_projection (if applicable)
  if(!(is.null(mat_proj))){
    extent(patches) <- extent(coords)
    projection(patches) <- mat_proj
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
  raw <- raster::raster(val_mat)

  # Specify coordinates and mat_projection (if applicable)
  if(!(is.null(mat_proj))){
    extent(raw) <- extent(coords)
    projection(raw) <- mat_proj
  }

  # Raster to points
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

  # Recreate dataframe
  df <- data.frame(raw)
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
      sumstats <-
        t(as.matrix(summary(patch_val[patch_val[,"G_bin"] == class, "val"])))
      colnames(sumstats) <-
        tolower(gsub(" ", "", gsub("[.]", "", colnames(sumstats))))
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
  pstats[, "grand_median"] <- median(val_mat, na.rm = TRUE)

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
