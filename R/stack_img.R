#' stack_imgs
#'
#' Helper function to create raster stack from a list of matrices.
#' @param img_list List of temperature matrices to convert.
#' @return Raster stack with one layer for each list element.
#' @examples
#' # Load raw data
#' raw_dat <- flir_raw$raw_dat
#' camera_params <- flir_raw$camera_params
#' metadata <- flir_metadata
#'
#' # Batch convert
#' img_list <- batch_convert(raw_dat, write_results = FALSE)
#' 
#' # Stack
#' img_stack <- stack_imgs(img_list)
#' raster::plot(img_stack)
#' @export

stack_imgs <- function(img_list) {
    img_stack <-
        lapply(img_list,
               function(x) raster::raster(
                   x,
                   xmn=0, xmx=ncol(x),
                   ymn=0, ymx=nrow(x)))
    img_stack <- raster::stack(img_stack)
    return(img_stack)
}

