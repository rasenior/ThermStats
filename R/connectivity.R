#' connectivity
#'
#' Calculate thermal connectivity and potential for temperature change.
#' @param img A numeric temperature matrix (such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}) or raster.
#' @param conn_threshold Climate threshold to use for calculation of thermal
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
#'  change (\code{diff_potential}) minus \code{conn_threshold}. Where
#'  this value is positive, connectivity of the starting pixel is sufficient to
#'  avoid the specified threshold of warming. See details.}
#' @details This measure of climate connectivity and potential for temperature
#' change is analogous to that described in
#' \href{https://doi.org/10.1073/pnas.1602817113}{McGuire et al. 2016}. The
#' basic premise is that there is a degree of change that organisms are seeking
#' to avoid, and when pixels are sufficiently heterogenous and well connected
#' organisms can move through the pixels to avoid deleterious temperature change.
#' @references
#' McGuire, J. L., Lawler, J. J., McRae, B. H., Nunez, T. A. and
#' Theobald, D. M. (2016), Achieving climate connectivity in a fragmented
#' landscape. PNAS, 113: 7195-7200.
#' \url{https://doi.org/10.1073/pnas.1602817113}
#' @importFrom rlang .data
#' @export
#' @examples
#' # Define matrix as FLIR thermal image
#' img <- flir11835$flir_matrix
#'
#' # Get connectivity
#' img_conn <-
#' connectivity(img = img,
#'              conn_threshold = 1.5)
#' head(img_conn)
#' 
#' # Plot the potential for temperature change
#' library(ggplot2)
#' ggplot(img_conn, 
#'        aes(x = x, y = y, fill = diff_potential))+
#'     geom_raster() +
#'     scale_fill_viridis_c()

connectivity <-
    function(img,
             conn_threshold = 1.5){
        
        message("Calculating thermal connectivity")
        
        # Determine whether matrix or raster
        if(class(img)[1] == "RasterLayer"){
            img <- raster::as.matrix(img)
        } else if(class(img)[1] == "SpatRaster"){
            img <- terra::as.matrix(img, wide = TRUE)
        }
        
        # Identify neighbours -----------------------------------------------------
        message("\t...identifying pixel neighbours")
        
        # Create a matrix of same size, with cell ID
        id_mat <- matrix(1:length(img),
                         nrow = nrow(img),
                         ncol = ncol(img))
        
        # Define row and column length
        n_cols <- ncol(id_mat)
        n_rows <- nrow(id_mat)
        
        # Pad matrix with NAs
        id_mat.pad <- rbind(NA, cbind(NA, id_mat, NA), NA)
        
        # Define row and column indices (not including NAs)
        col_ind <- 2:(n_cols + 1)
        row_ind <- 2:(n_rows + 1)
        
        # Identify neighbours
        # (Only vertical and horizontal neighbours)
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
        val_df <- reshape2::melt(img,
                                 varnames = c("y", "x"),
                                 value.name = "val")
        # Remove 'V' from x column
        val_df[,"x"] <- as.integer(gsub("V","",val_df[,"x"]))
        
        # Assign pixel ID (row index)
        val_df$pixel <- as.numeric(row.names(val_df))
        
        # Bind nbr and value dfs
        nbr <- cbind(nbr, val_df)
        
        # Wide to long format (one row for each neighbour)
        nbr <- stats::reshape(nbr,
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
        
        # Drop NAs
        val_df <- val_df[!(is.na(val_df[,"val"])),]
        nbr <- nbr[!(is.na(nbr[,"val1"])),]
        nbr <- nbr[!(is.na(nbr[,"val2"])),]
        
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
        message("\t...tracing pixels to coolest destination pixel")
        
        # Identify all the neighbouring destination pixels for each unique pixel
        # Join on origin pixel
        connectsto <-
            dplyr::left_join(val_df, nbr[,c("origin_pixel", "dest_pixel")], 
                             by = c("pixel" = "origin_pixel"))[,c("pixel", "dest_pixel")]
        # Summarise by origin pixel
        connectsto <- 
            dplyr::summarise(
                dplyr::group_by(connectsto, .data$pixel), dest = list(.data$dest_pixel))
        # Name by origin pixel
        names(connectsto[["dest"]]) <- connectsto[["pixel"]]
        # List only
        connectsto <- connectsto[["dest"]]
        
        # Create vector of unique vals
        uniquevals <- sort(unique(val_df$val))
        
        #set up output file
        val_df$dest_pixel <- NA
        val_df$dest_val <- NA
        val_df$inter_pixel <- NA
        
        # Define list of pixels and vals to update as pixels are connected to each other
        running <- val_df[,c("pixel", "val")]
        
        # Loop through each unique temperature, from colder to warmer
        for (j in seq_along(uniquevals)){
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
                if(any(is.na(connectsto[[ii]]))){
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
        val_df$therm_conn <- val_df$diff_potential - conn_threshold
        
        # Return ------------------------------------------------------------------
        return(val_df)
        
    }







