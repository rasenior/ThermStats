#' crop_mat
#'
#' Crops temperature matrix to the desired area.
#' @param val_mat A numeric matrix, such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}.
#' @return A cropped version of the input matrix.
#' @details Requires user input to iteratively refine the size and location of
#' the cropping area.
#' @examples
#' # Crop thermal image of wasp nest to the nest only
#' # N.B. For this example, a good approximation is an ellipse with
#' # origin coordinates at x = 350, y = 290, x-axis radius of 145
#' # and y-axis radius of 190.
#'
#' cropped <- crop_mat(val_mat = waspnest_mat)
#'
#' # Get patches
#' crop_patches <- get_patches(cropped)
#'
#' # Plot with patches
#' plot_patches(df = crop_patches$df,
#'              patches = crop_patches$patches,
#'              plot_distribution = FALSE)
#'
#' @export
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'

crop_mat <- function(val_mat){

  # Get matrix dimensions
  nrows <- nrow(val_mat)
  ncols <- ncol(val_mat)

  # Rasterise matrix
  val_raster <-
    raster::raster(val_mat,
                   xmn=1, xmx=ncols,
                   ymn=1, ymx=nrows)

  # Plot
  if(requireNamespace("viridisLite", quietly = TRUE)){
    raster::plot(val_raster, col = viridisLite::magma(255))
  }else{
    raster::plot(val_raster, col = heat.colors(255))
  }

  # Ask user for cropping shape
  shape <- menu(choices = c("ellipse", "rectangle"),
                title = "Crop with an ellipse or rectangle?")

  # Ask user for parameters
  args <- define_params()
  # Keep asking until all parameters are either numeric or empty
  while(any(sapply(args, is.null))){
    args <- define_params()
  }
  names(args) <- c("x", "y", "radius.x", "radius.y")

  if(shape == 1){
    # Create elliptical polygon
    poly <-
      DescTools::DrawRegPolygon(x = args$x,
                                y = args$y,
                                radius.x = args$radius.x,
                                radius.y = args$radius.y,
                                rot = 0,
                                nv = 100,
                                col = "transparent",
                                plot = TRUE)
  }else{

    xleft <- args$x - args$radius.x
    ybottom <- args$y - args$radius.y
    xright <- args$x + args$radius.x
    ytop <- args$y + args$radius.y

    rect(xleft = xleft,
         ybottom = ybottom,
         xright = xright,
         ytop = ytop)

    poly <- list(x = c(xleft, xleft, xright, xright),
                 y = c(ybottom, ytop, ytop, ybottom))

  }

  # User input
  check <- menu(choices = c("yes", "no"),
                title = "Happy with this cropping area?")

  while(check == 2){

    # Ask user for parameters
    new_args <- define_params()
    while(any(sapply(new_args, is.null))){
      new_args <- define_params()
    }
    names(new_args) <- c("x", "y", "radius.x", "radius.y")

    new_ind <- which(!(is.na(new_args)))

    args <- replace(args, new_ind,new_args[new_ind])

    # Create the ellipse again
    plot.new()
    if(requireNamespace("viridisLite", quietly = TRUE)){
      raster::plot(val_raster, col = viridisLite::magma(255))
    }else{
      raster::plot(val_raster, col = heat.colors(255))
    }

    if(shape == 1){
      poly <-
        DescTools::DrawRegPolygon(x = args$x,
                                  y = args$y,
                                  radius.x = args$radius.x,
                                  radius.y = args$radius.y,
                                  rot = 0,
                                  nv = 100,
                                  col = "transparent",
                                  plot = TRUE)
    }else{

      xleft <- args$x - args$radius.x
      ybottom <- args$y - args$radius.y
      xright <- args$x + args$radius.x
      ytop <- args$y + args$radius.y

      rect(xleft = xleft,
           ybottom = ybottom,
           xright = xright,
           ytop = ytop)

      poly <- list(x = c(xleft, xleft, xright, xright),
                   y = c(ybottom, ytop, ytop, ybottom))

    }

    # User input
    check <- menu(choices = c("yes", "no"),
                  title = "Happy with this cropping area?")

  }

  p = sp::Polygon(cbind(poly$x, poly$y))
  ps = sp::Polygons(list(p),1)
  sps = sp::SpatialPolygons(list(ps))

  # Crop and mask
  val_raster <- raster::crop(val_raster, raster::extent(sps))
  val_raster <- raster::mask(val_raster, sps)

  # Return as matrix
  val_mat <- raster::as.matrix(val_raster)
  # Flip
  val_mat <-
    Thermimage::mirror.matrix(Thermimage::rotate180.matrix(val_mat))

  return(val_mat)

}

# Function to request multiple user inputs sequentially
readlines <- function(...) {
  lapply(list(...), readline)
}

define_params <- function(){
  # Ask user for parameters
  new_args <- readlines("x coordinate of origin: ",
                        "y coordinate of origin: ",
                        "x-axis radius: ",
                        "y-axis radius: ")

  # Coerce to numeric
  new_args <- lapply(new_args, function(x){
    tryCatch(
      {
        as.numeric(x)
      },
      warning=function(w) {
        message("Non-numeric value specified: ", x, "\n")
      })
  })
  return(new_args)
}






