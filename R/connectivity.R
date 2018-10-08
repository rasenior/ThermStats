#' connectivity
#'
#' Calculate potential for valerature change and climate connectivity.
#' @param val_mat A numeric matrix.
#' @param threshold Climate threshold to use for calculation of climate
#' connectivity (i.e. the amount of change that organisms would be seeking
#' to avoid).
#' @return A dataframe (one row for each pixel) containing:
#'  \item{x,y}{The original spatial location of the pixel}
#'  \item{val}{The pixel value}
#'  \item{pixel}{The unique id given to the pixel}
#'  \item{dest_pixel}{The id of the final destination pixel}
#'  \item{dest_val}{The value of final destination pixel}
#'  \item{inter_pixel}{The intermediate pixels traversed from origin to
#'  destination pixel}
#'  \item{diff_potential}{The potential for change achieved by following
#'  gradient from hotter to cooler pixels}
#'  \item{clim_conn}{Climate connectivity, calculated as the maximum potential
#'  change ('diff_potential') minus the climate threshold. Where this value is
#'  positive, connectivity from the starting pixel is sufficient to avoid the
#'  specified threshold of climate warming. See details.}
#'  @details This measure of climate connectivity and potential for temperature
#'  change is analogous to that described in
#'  \href{https://doi.org/10.1073/pnas.1602817113}{McGuire et al. 2016}. The
#'  basic premise is there is a degree of change that organisms are seeking to
#'  avoid, and where pixels are sufficiently heterogenous and well connected
#'  simply moving through the pixels can allow organisms to avoid delterious
#'  change.
#'  @references
#'  McGuire, J. L., Lawler, J. J., McRae, B. H., Nuñez, T. A. and
#'  Theobald, D. M. (2016), Achieving climate connectivity in a fragmented
#'  landscape. PNAS, 113: 7195-7200.
#' \url{https://doi.org/10.1073/pnas.1602817113}
#'
#' @examples
#'
#' # FLIR valerature matrix ---------------------------------------------------
#'
#' # Define individual matrix
#' val_mat <- flir11835$flir_matrix
#'
#' # Get connectivity
#' mat_conn <-
#' connectivity(val_mat = val_mat,
#'              threshold = 1.5)
#' head(mat_conn)
#'
#' @export

connectivity <-
  function(val_mat,
           threshold = 1.5){

    # Identify neighbours -----------------------------------------------------

    # Create a matrix of same size, with cell ID
    id_mat <- matrix(1:length(val_mat),
                     nrow = nrow(val_mat),
                     ncol = ncol(val_mat))

    # Define row and column length
    n_cols <- ncol(id_mat)
    n_rows <- nrow(id_mat)

    # Pad matrix with NAs
    id_mat.pad <- rbind(NA, cbind(NA, id_mat, NA), NA)

    # Define row and column indices (not including NAs)
    col_ind <- 2:(n_cols + 1)
    row_ind <- 2:(n_rows + 1)

    # Identify neighbours
    # Including only vertical and horizontal neighbours
    nbr <-
      as.data.frame(
        cbind(N  = as.vector(id_mat.pad[row_ind - 1, col_ind    ]),
              E  = as.vector(id_mat.pad[row_ind    , col_ind + 1]),
              S  = as.vector(id_mat.pad[row_ind + 1, col_ind    ]),
              W  = as.vector(id_mat.pad[row_ind    , col_ind - 1]))
      )

    # Add pixel ID column
    nbr$pixel <- as.numeric(row.names(nbr))

    # Coerce value matrix to dataframe
    val_df <- reshape2::melt(val_mat,
                             varnames = c("y", "x"),
                             value.name = "val")

    # Bind nbr and value dfs
    nbr <- cbind(nbr, val_df)

    # Wide to long format (one row for each neighbour)
    nbr <- gather(nbr, key = "direction", value = "nbrs", c("N", "E", "S", "W"))

    # Remove NAs & direction col
    nbr <- nbr[!(is.na(nbr$nbrs)), c("y","x","pixel", "nbrs", "val")]

    # Re-order
    nbr <- nbr[order(nbr$pixel, nbr$nbrs),]

    # Rename
    colnames(nbr) <- c("y","x","pixel1", "pixel2","val1")

    # Assign valerature of nbr pixel
    nbr$val2 <- val_df[nbr$pixel2,"val"]

    # Reduce to one side of neighbour relationship only
    nbr <- nbr[(nbr$pixel2 - nbr$pixel1) > 0,]
    row.names(nbr) <- NULL

    # Assign pixel ID (row index)
    val_df$pixel <- as.numeric(row.names(val_df))

    # Neighbour relationship --------------------------------------------------

    # Determine, for each pair of neighbouring pixeles, which of the two is
    # the hotter 'origin' pixel and which is the cooler 'destination' pixel
    nbr$origin_pixel <- NA
    nbr$dest_pixel <- NA
    nbr$origin_val <- NA
    nbr$dest_val <- NA

    for (i in 1:nrow(nbr)){
      # If val1 is more than val2, pixel1 is the origin and pixel 2 is
      # the destination
      if(nbr$val1[i] > nbr$val2[i]){
        nbr$origin_pixel[i] <- nbr$pixel1[i]
        nbr$origin_val[i] <- nbr$val1[i]
        nbr$dest_pixel[i] <-nbr$pixel2[i]
        nbr$dest_val[i] <- nbr$val2[i]
        # If val2 is more than val1, pixel2 is the origin and pixel 1 is
        # the destination
      }else{
        nbr$origin_pixel[i]<- nbr$pixel2[i]
        nbr$origin_val[i] <- nbr$val2[i]
        nbr$dest_pixel[i]<-nbr$pixel1[i]
        nbr$dest_val[i] <- nbr$val1[i]
      }
    }

    # Remove unnecessary variables
    nbr <- nbr[,c("origin_pixel", "dest_pixel", "origin_val", "dest_val")]

    # Determine final destination ---------------------------------------------

    # Identify all the neighbouring destination pixeles for each origin pixel
    connectsto <-
      sapply(1:nrow(val_df), function(x){
        nbr$dest_pixel[nbr$origin_pixel == val_df[x,"pixel"]]
      })

    # Create vector of unique vals
    uniquevals <- sort(unique(val_df$val))

    #set up output file
    val_df$dest_pixel <- NA
    val_df$dest_val <- NA
    val_df$inter_pixel <- NA

    # Define valorary list of pixels and vals to update as pixels are
    # connected to each other
    running <- val_df[,c("pixel", "val")]

    # Loop through each unique valerature, from colder to warmer
    for (j in 1:length(uniquevals)){
      # Define the focal unique valerature
      warmer <- uniquevals[j]
      # Define the indices of pixels that correspond to the focal valerature
      inds <- which(running[,"val"] == warmer)

      # Iterate over the indices
      for (k in 1:(length(inds))){
        ii <- inds[k]
        # If the focal pixel associated with this index does not connect to
        # any other pixel, the final destination pixel is the same as the focal
        # pixel and its final val is the same as its starting val
        if(length(connectsto[[ii]]) == 0){
          val_df$dest_val[ii]<- warmer
          val_df$dest_pixel[ii]<- running[ii, "pixel"]
          # If it does connect to another pixel...
        }else{
          # Retrieve indices of the destination pixels
          topixelsinds <- which(val_df$pixel %in%  connectsto[[ii]])
          # Calculate which of the destination pixels has the lowest valerature
          t <- min(running[topixelsinds, "val"])
          # Retrieve indices of the destination pixels that correspond to
          # this minimum valerature
          a <- which(running[topixelsinds, "val"] == t)

          if(length(a)==1){
            # If there is only one coldest pixel, index that destination (colder) pixel &
            minind <- topixelsinds[a]
          }else{
            # If there is more than one colder pixel w/ = valeratures, arbitrarily select the first one
            minind <- topixelsinds[a[1]]
          }

          # Assign the min destination val as the final val. of the focal pixel
          val_df$dest_val[ii] <- t
          # Assign the final pixel of the focal pixel as its coldest (destination) pixel
          val_df$dest_pixel[ii] <- running[minind, "pixel"]
          # Assign the final pixel and its intermediate pixeles
          inter_pixel <- val_df[minind, "inter_pixel"]

          if(!(is.na(inter_pixel))){
            val_df$inter_pixel[ii] <-
              paste(val_df[minind, "pixel"],
                    inter_pixel[!(is.na(inter_pixel))],
                    sep = ";")
          }else{
            val_df$inter_pixel[ii] <- val_df[minind, "pixel"]
          }
          # Assign the final val of the focal pixel as the new minimum valerature
          running[ii, "val"] <- t
          # Have new colder (destination) pixel replace the origin pixel in runningpixel
          running[ii, "pixel"] <- running[minind, "pixel"]
        }
      }
    }


    # Calculate potential valerature diff ------------------------------------

    # This is the maximum val diff that can be achieved by traversing gradient
    # of hotter to cooler pixeles
    val_df$diff_potential <- val_df$val - val_df$dest_val

    # Is this sufficient to avoid climate warming?
    val_df$clim_conn <- val_df$diff_potential - threshold

    # Return ------------------------------------------------------------------
    return(val_df)

}







