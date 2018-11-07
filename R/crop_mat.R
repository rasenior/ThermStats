#' crop_mat
#'
#' Crops temperature matrix to the desired area.
#' @param val_mat A numeric matrix, such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}.
#' @examples
#' path <- "C:\\Users\\Rebecca\\Google Drive\\PhD\\Ch3\\data\\flir_sample\\Test.jpg"
#' img <- readflirJPG(path)
#' settings <-
#' flirsettings(path)$Info %>%
#' dplyr::bind_rows()
#' val_mat <- raw2temp(raw = img,
#'                     E = settings$Emissivity,
#'                     OD = settings$ObjectDistance,
#'                     RTemp = settings$ReflectedApparentTemperature,
#'                     ATemp = settings$AtmosphericTemperature,
#'                     IRWTemp = settings$IRWindowTemperature,
#'                     IRT = settings$IRWindowTransmission,
#'                     RH = settings$RelativeHumidity,
#'                     PR1 = settings$PlanckR1,
#'                     PB = settings$PlanckB,
#'                     PF = settings$PlanckF,
#'                     PO = settings$PlanckO,
#'                     PR2 = settings$PlanckR2)
#'
#' # 350, 290, 145,190
#' cropped <- crop_mat(val_mat = val_mat)
#' crop_patches <- get_patches(cropped)
#' plot_patches(df = crop_patches$df,
#' patches = crop_patches$patches)
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
  raster::plot(val_raster)

  # Ask user for cropping shape
  shape <- menu(choices = c("ellipse", "rectangle"),
                title = "Crop with an ellipse or rectangle?")
  # Ask user for parameters
  args <- readlines("x coordinate of origin: ",
                   "y coordinate of origin: ",
                   "x-axis radius: ",
                   "y-axis radius: ")
  args <- lapply(args, as.numeric)
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
  check <- readline(prompt="Happy with this? ")

  while(tolower(check) == "no"){

    # Ask user for parameters
    new_args <- readlines("x coordinate of origin: ",
                      "y coordinate of origin: ",
                      "x-axis radius: ",
                      "y-axis radius: ")
    new_args <- lapply(new_args, as.numeric)
    names(new_args) <- c("x", "y", "radius.x", "radius.y")
    new_ind <- which(!(is.na(new_args)))

    args <- replace(args, new_ind,new_args[new_ind])

    # Create the ellipse again
    plot.new()
    # Plot
    raster::plot(val_raster)

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
    check <- readline(prompt="Satisfied? ")

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

readlines <- function(...) {
  lapply(list(...), readline)
}
