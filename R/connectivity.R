#' connectivity
#'
#' Calculate climate connectivity and potential for temperature change.
#' @param val_mat A numeric matrix.
#' @param conn_thresh Climate threshold to use for calculation of climate
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
#'  \item{therm_conn}{Thermal connectivity, calculated as the maximum potential
#'  change (\code{diff_potential}) minus \code{conn_thresh}. Where
#'  this value is positive, connectivity of the starting pixel is sufficient to
#'  avoid the specified conn_thresh of warming. See details.}
#' @details This measure of climate connectivity and potential for temperature
#' change is analogous to that described in
#' \href{https://doi.org/10.1073/pnas.1602817113}{McGuire et al. 2016}. The
#' basic premise is that there is a degree of change that organisms are seeking
#' to avoid; where pixels are sufficiently heterogenous and well connected
#' organisms can move through the pixels to avoid delterious change.
#' @references
#' McGuire, J. L., Lawler, J. J., McRae, B. H., Nunez, T. A. and
#' Theobald, D. M. (2016), Achieving climate connectivity in a fragmented
#' landscape. PNAS, 113: 7195-7200.
#' \url{https://doi.org/10.1073/pnas.1602817113}
#'
#' @examples
#' # Define matrix as FLIR thermal image
#' val_mat <- flir11835$flir_matrix
#'
#' # Get connectivity
#' mat_conn <-
#' connectivity(val_mat = val_mat,
#'              conn_thresh = 1.5)
#' head(mat_conn)
#'
#' @export

connectivity <-
  function(val_mat,
           conn_thresh = 1.5){

    # Identify neighbours -----------------------------------------------------
    message("Identifying pixel neighbours")

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
        cbind(nbrs_N  = as.vector(id_mat.pad[row_ind - 1, col_ind    ]),
              nbrs_E  = as.vector(id_mat.pad[row_ind    , col_ind + 1]),
              nbrs_S  = as.vector(id_mat.pad[row_ind + 1, col_ind    ]),
              nbrs_W  = as.vector(id_mat.pad[row_ind    , col_ind - 1]))
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
    nbr <- reshape(nbr,
                   varying = 1:4,
                   timevar = "direction",
                   idvar = "pixel",
                   direction = "long",
                   sep = "_")

    # Remove NAs & direction col
    nbr <- nbr[!(is.na(nbr$nbrs)), c("y","x","pixel", "nbrs", "val")]

    # Re-order
    nbr <- nbr[order(nbr$pixel, nbr$nbrs),]

    # Rename
    colnames(nbr) <- c("y","x","pixel1", "pixel2","val1")

    # Assign temperature of nbr pixel
    nbr$val2 <- val_df[nbr$pixel2,"val"]

    # Reduce to one side of neighbour relationship only
    nbr <- nbr[(nbr$pixel2 - nbr$pixel1) > 0,]
    row.names(nbr) <- NULL

    # Assign pixel ID (row index)
    val_df$pixel <- as.numeric(row.names(val_df))

    # Neighbour relationship --------------------------------------------------

    # Determine, for each pair of neighbouring pixels, which of the two is
    # the hotter 'origin' pixel and which is the cooler 'destination' pixel

    # If val1 is more than val2: pixel1 is the origin
    nbr$origin_pixel <-
      ifelse(nbr$val1 > nbr$val2,
             nbr$pixel1,
             nbr$pixel2)
    # If val1 is less than or equal to val2: pixel1 is the destination
    nbr$dest_pixel <-
      ifelse(nbr$val1 <= nbr$val2,
             nbr$pixel1,
             nbr$pixel2)

    # Also assign origin & dest values (same method as above)
    nbr$origin_val <-
      ifelse(nbr$val1 > nbr$val2,
             nbr$val1,
             nbr$val2)
    nbr$dest_val <-
      ifelse(nbr$val1 <= nbr$val2,
             nbr$val1,
             nbr$val2)

    # Remove unnecessary variables
    nbr <- nbr[,c("origin_pixel", "dest_pixel", "origin_val", "dest_val")]

    # Determine final destination ---------------------------------------------
    message("Tracing pixels to coolest destination pixel")

    # Identify all the neighbouring destination pixels for each unique pixel
    connectsto <-
      sapply(unique(val_df[,"pixel"]), function(x){
        nbr$dest_pixel[nbr$origin_pixel == x]
      })

    # Create vector of unique vals
    uniquevals <- sort(unique(val_df$val))

    #set up output file
    val_df$dest_pixel <- NA
    val_df$dest_val <- NA
    val_df$inter_pixel <- NA

    # Define list of pixels and vals to update as pixels are connected to each other
    running <- val_df[,c("pixel", "val")]

    # Loop through each unique temperature, from colder to warmer
    for (j in 1:length(uniquevals)){
      # Define the focal unique temperature
      warmer <- uniquevals[j]
      # Define the indices of pixels that correspond to the focal temperature
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
          # Calculate which of the destination pixels has the lowest temperature
          t <- min(running[topixelsinds, "val"])
          # Retrieve indices of the destination pixels that correspond to
          # this minimum temperature
          a <- which(running[topixelsinds, "val"] == t)

          if(length(a)==1){
            # If there is only one coldest pixel, index that destination (colder) pixel &
            minind <- topixelsinds[a]
          }else{
            # If there is more than one colder pixel w/ = temperatures, arbitrarily select the first one
            minind <- topixelsinds[a[1]]
          }

          # Assign the min destination val as the final val. of the focal pixel
          val_df$dest_val[ii] <- t
          # Assign the final pixel of the focal pixel as its coldest (destination) pixel
          val_df$dest_pixel[ii] <- running[minind, "pixel"]
          # Assign the final pixel and its intermediate pixels
          inter_pixel <- val_df[minind, "inter_pixel"]

          if(!(is.na(inter_pixel))){
            val_df$inter_pixel[ii] <-
              paste(val_df[minind, "pixel"],
                    inter_pixel[!(is.na(inter_pixel))],
                    sep = ";")
          }else{
            val_df$inter_pixel[ii] <- val_df[minind, "pixel"]
          }
          # Assign the final val of the focal pixel as the new minimum temperature
          running[ii, "val"] <- t
          # Have new colder (destination) pixel replace the origin pixel in runningpixel
          running[ii, "pixel"] <- running[minind, "pixel"]
        }
      }
    }

    # Calculate potential temperature diff ------------------------------------

    # This is the maximum val diff that can be achieved by traversing gradient
    # of hotter to cooler pixels
    val_df$diff_potential <- val_df$val - val_df$dest_val

    # Is this sufficient to avoid climate warming?
    val_df$therm_conn <- val_df$diff_potential - conn_thresh

    # Return ------------------------------------------------------------------
    return(val_df)

  }







