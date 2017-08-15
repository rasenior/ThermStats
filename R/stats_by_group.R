#' stats_by_group
#'
#' Calculate thermal statistics across photos within groups.
#' @param in_dir Path to directory where thermal images are stored.
#' @param write_results Should the results be written as an Rdata file? Defaults to true.
#' @param out_dir Path to directory where output Rdata file will be stored. Defaults to working directory.
#' @return A list containing:
#'  \item{raw_dat}{A list with one element per input thermal image. Each element is a numeric matrix of the
#' raw infrared data.}
#'  \item{camera_params}{A dataframe of callibration constants unique to each camera.}
#' @examples
#' # Batch extract four FLIR thermal images included in this package.
#' results <- batch_extract(system.file("extdata", package = "PatchStatsFLIR"), write_results = FALSE)
#' @export
#'
# Define function to return stats for each grouping
stats_by_group <- function(metadata,flir_list,photo_ID = "photo_no",
                         subset_var,subset_val, round_val,...){

  # Setup ----------------------------------------------------------------------
  sub_photos <- as.character(metadata[metadata[,subset_var]==subset_val,photo_ID])

  n_photos <- length(sub_photos)

  # Subset matrices list by the desired photo numbers
  sub_list <- flir_list[as.character(sub_photos)]

  # Rounding
  sub_list<-
    lapply(sub_list, function(x){
      round_val * round(x / round_val)
    })

  # Pixel statistics -----------------------------------------------------------
  # -> these statistics are calculated across all pixels of all subset photos

  # Bind all data together
  all_pixels <- as.vector(do.call("rbind",sub_list))

  pixel_stats <- multi.sapply(all_pixels,...)

  # colnames(pixel_stats) <- eval
  colnames(pixel_stats) <-
    paste(match.call(expand.dots = FALSE)$...)

  # Patch statistics -----------------------------------------------------------
  # -> these statistics are calculated for hot and cold patches

  # For each matrix in the list, calculate spatial statistics
  patch_stats <-
    lapply(1:length(sub_list),
           function(x) get_patches(flir_matrix = sub_list[[x]],
                                   photo_no = sub_photos[x],
                                   return_vals = "patch_stats"))

  # Bind as dataframe
  patch_stats <- do.call("rbind", patch_stats)

  ### Aggregate results over all photos to give one statistic for the subset
  # These variables should be summed across photos
  sum_vars <-
    c("hot_px_no","hot_patch_no","hot_obs_edges","hot_max_edges","hot_non_edges",
      "cold_px_no","cold_patch_no","cold_obs_edges","cold_max_edges","cold_non_edges")

  patch_stats<-
    c(apply(patch_stats[,sum_vars],
            MARGIN = 2,
            FUN = sum,
            na.rm=TRUE),
      # Max patch temp is the max across all photos
      apply(patch_stats[,c("hot_patch_max_temp",
                           "cold_patch_max_temp")],
            MARGIN = 2,
            FUN = max,
            na.rm=TRUE),
      # Min patch temp is the min across all photos
      apply(patch_stats[,c("hot_patch_min_temp",
                           "cold_patch_min_temp")],
            MARGIN = 2,
            FUN = min,
            na.rm=TRUE))
  # Reformat
  patch_stats <- data.frame(t(patch_stats))

  # Return results -------------------------------------------------------------

  # Bind all results
  results <- cbind(n_photos, pixel_stats, patch_stats)
  # Add ID
  results[,subset_var] <- subset_val

  return(results)
}

# Function to apply multiple functions to a vector
multi.sapply <- function(...) {
  # Reads in all arguments passed to function, including data
  arglist <- match.call(expand.dots = FALSE)$...

  # Deparse argument names
  var.names <- sapply(arglist, deparse)

  # For all arguments that had function names specified,
  # substitue name from deparsed expression by the given name
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]

  # Evaluate the expressions given in arguments;
  # go two generations back as we apply eval.parent
  # within lapply function
  arglist <- lapply(arglist, eval.parent, n = 2)

  # First argument contains data set, so remove it from the list
  data <- arglist[[1]]
  arglist[[1]] <- NULL

  # Apply every function
  val<-sapply(X=arglist,
              FUN=function(arglist) arglist(data))

  # Re-format to dataframe
  val <- as.data.frame(t(val))
  rownames(val) <- NULL

  return(val = val)
}

