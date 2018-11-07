
library(ThermStats)
library(Thermimage)
library(dplyr)

path <- "C:\\Users\\Rebecca\\Google Drive\\PhD\\Ch3\\data\\flir_sample\\Test.jpg"
img <- readflirJPG(path)
settings <-
  flirsettings(path)$Info %>%
  bind_rows()

readlines <- function(...) {
  lapply(list(...), readline)
}

val_mat <- raw2temp(raw = img,
                    E = settings$Emissivity,
                    OD = settings$ObjectDistance,
                    RTemp = settings$ReflectedApparentTemperature,
                    ATemp = settings$AtmosphericTemperature,
                    IRWTemp = settings$IRWindowTemperature,
                    IRT = settings$IRWindowTransmission,
                    RH = settings$RelativeHumidity,
                    PR1 = settings$PlanckR1,
                    PB = settings$PlanckB,
                    PF = settings$PlanckF,
                    PO = settings$PlanckO,
                    PR2 = settings$PlanckR2)


# Function to crop thermal image to desired area
crop_mat <- function(val_mat,
                     shape = "ellipse",
                     x,
                     y,
                     radius.x,
                     radius.y){

  # Check that shape specified correctly
  if(!(any(c("ellipse", "rectangle") == shape))){
    stop("Invalid shape, please specify either 'ellipse' or 'rectangle'")
  }

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

  shape <- readline("Crop with an ellipse or rectangle: ")


  if(shape == "ellipse"){
    # Create elliptical polygon
    poly <-
      DescTools::DrawRegPolygon(x = x,
                                y = y,
                                radius.x = radius.x,
                                radius.y = radius.y,
                                rot = 0,
                                nv = 100,
                                col = "transparent",
                                plot = TRUE)
  }else{

    xleft <- x - radius.x
    ybottom <- y - radius.y
    xright <- x + radius.x
    ytop <- y + radius.y

    rect(xleft = xleft,
         ybottom = ybottom,
         xright = xright,
         ytop = ytop)

    poly <- list(x = c(xleft, xleft, xright, xright),
                 y = c(ybottom, ytop, ytop, ybottom))

  }

  # User input
  check <- readline(prompt="Satisfied? ")

  while(tolower(check) == "no"){
    args <- c("x", "y","radius.x","radius.y")
    # Allow user to update parameters
    argind <- menu(choices = args,
                   title = "Please select the parameter you would like to update")
    argnew <- as.numeric(readline(prompt="Please define a new value for this parameter: "))

    old_args <- c(x, y, radius.x, radius.y)
    new_args <- replace(old_args, argind,argnew)
    names(new_args) <- args

    # Create the ellipse again
    plot.new()
    # Plot
    raster::plot(val_raster)

    if(shape == "ellipse"){
      # Create elliptical polygon
      poly <- DescTools::DrawRegPolygon(x = new_args["x"],
                                        y = new_args["y"],
                                        radius.x = new_args["radius.x"],
                                        radius.y = new_args["radius.y"],
                                        rot = 0,
                                        nv = 100,
                                        col = "transparent",
                                        plot = TRUE)
    }else{

      xleft <- new_args["x"] - new_args["radius.x"]
      ybottom <- new_args["y"] - new_args["radius.y"]
      xright <- new_args["x"] + new_args["radius.x"]
      ytop <- new_args["y"] + new_args["radius.y"]

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

  return(val_raster)

}

crop_bb <-
  crop_mat(shape = "ellipse",
           val_mat = val_mat,
           x = 320,
           y = 240,
           radius.x = 100,
           radius.y = 400)
