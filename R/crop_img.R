#' crop_img
#'
#' Crops temperature data to the desired area.
#' @param img A numeric temperature matrix (such as that returned from
#' \code{Thermimage::}\code{\link[Thermimage]{raw2temp}}) or raster.
#' @param crop_poly If polygon of cropping area is already defined, supply here.
#' Defaults to NULL.
#' @param rtn_format Desired format ('matrix' or 'raster') of the area returned
#' after cropping. Defaults to 'matrix'.
#' @return A cropped version of the input image.
#' @details Unless the cropping polygon is already available, function requires 
#' user input to iteratively refine the size, location and angle (optional) of 
#' the cropping area.
#' @examples
#' # Crop thermal image of wasp nest to the nest only
#' # N.B. For this example, a good approximation is an ellipse with
#' # origin coordinates at x = 350, y = 290, x-axis radius of 145
#' # and y-axis radius of 190.
#' 
#' \dontrun{
#' cropped <- crop_img(img = waspnest_mat)
#'
#' # Get patches
#' crop_patches <- get_patches(cropped)
#'
#' # Plot with patches
#' plot_patches(df = crop_patches$df,
#'              patches = crop_patches$patches,
#'              plot_distribution = FALSE)
#' }
#'
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @export

crop_img <- function(img, crop_poly = NULL, rtn_format = "matrix"){
    
    # Get dimensions
    nrows <- nrow(img)
    ncols <- ncol(img)
    
    # If polygon supplied cropping is easy...
    if(!(is.null(crop_poly))){
        
        # If matrix, coerce to raster
        if(is.matrix(img)){
            img <- raster::raster(img,
                                  xmn=1, xmx=ncols,
                                  ymn=1, ymx=nrows)
        }
        
        # Crop
        img <- raster::mask(img, crop_poly)
        
        # Return
        if(rtn_format == "matrix"){
            # Coerce to matrix
            img <- raster::as.matrix(img)
            # Flip
            img <-
                Thermimage::mirror.matrix(Thermimage::rotate180.matrix(img))
            return(img)
        }else{
            return(img)
        }
        
        # If polygon not supplied, user must iteratively define cropping area
    }else{
        
        # Rasterise matrix
        if(is.matrix(img)){
            img <-
                raster::raster(img,
                               xmn=1, xmx=ncols,
                               ymn=1, ymx=nrows)
        }
        
        # Plot
        if (requireNamespace("viridisLite", quietly = TRUE)) {
            raster::plot(img, col = viridisLite::magma(255))
        }else{
            raster::plot(img, col = grDevices::heat.colors(255))
        }
        
        # Ask user for cropping shape
        shape <- utils::menu(choices = c("ellipse", "rectangle", "polygon"),
                             title = "Crop with an ellipse, rectangle or polygon?")
        
        # Ask user for parameters
        args <- define_params(shape)
        # Keep asking until all parameters are either numeric or empty
        while(any(sapply(args, is.null))){
            args <- define_params()
        }
        
        if(shape != 2){
            names(args) <- c("x", "y", "radius.x", "radius.y", "angle")
        }else names(args) <- c("x", "y", "radius.x", "radius.y")
        
        # Elliptical polygon
        if(shape == 1){
            poly <-
                DescTools::DrawRegPolygon(x = args$x,
                                          y = args$y,
                                          radius.x = args$radius.x,
                                          radius.y = args$radius.y,
                                          rot = deg2rad(args$angle),
                                          nv = 100,
                                          col = "transparent",
                                          plot = TRUE)
            # Rectangle
        }else if(shape == 2){
            xleft <- args$x - args$radius.x
            ybottom <- args$y - args$radius.y
            xright <- args$x + args$radius.x
            ytop <- args$y + args$radius.y
            
            graphics::rect(xleft = xleft,
                 ybottom = ybottom,
                 xright = xright,
                 ytop = ytop)
            
            poly <- list(x = c(xleft, xleft, xright, xright),
                         y = c(ybottom, ytop, ytop, ybottom))
            
            # Polygon
        }else{
            poly <-
                DescTools::DrawRegPolygon(x = args$x,
                                          y = args$y,
                                          radius.x = args$radius.x,
                                          radius.y = args$radius.y,
                                          rot = deg2rad(args$angle),
                                          nv = 4,
                                          col = "transparent",
                                          plot = TRUE)
        }
        
        # User input
        check <- utils::menu(choices = c("yes", "no"),
                             title = "Happy with this cropping area?")
        
        while(check == 2){
            
            # Ask user for parameters
            new_args <- define_params(shape)
            while(any(sapply(new_args, is.null))){
                new_args <- define_params()
            }
            
            if(shape != 2){
                names(new_args) <- c("x", "y", "radius.x", "radius.y", "angle")
            }else names(new_args) <- c("x", "y", "radius.x", "radius.y")
            
            new_ind <- which(!(is.na(new_args)))
            
            args <- replace(args, new_ind,new_args[new_ind])
            
            # Create the ellipse again
            graphics::plot.new()
            if(requireNamespace("viridisLite", quietly = TRUE)){
                raster::plot(img, col = viridisLite::magma(255))
            }else{
                raster::plot(img, col = grDevices::heat.colors(255))
            }
            
            # Elliptical polygon
            if (shape == 1) {
                poly <-
                    DescTools::DrawRegPolygon(x = args$x,
                                              y = args$y,
                                              radius.x = args$radius.x,
                                              radius.y = args$radius.y,
                                              rot = deg2rad(args$angle),
                                              nv = 100,
                                              col = "transparent",
                                              plot = TRUE)
                # Rectangle
            }else if (shape == 2) {
                xleft <- args$x - args$radius.x
                ybottom <- args$y - args$radius.y
                xright <- args$x + args$radius.x
                ytop <- args$y + args$radius.y
                
                graphics::rect(xleft = xleft,
                               ybottom = ybottom,
                               xright = xright,
                               ytop = ytop)
                
                poly <- list(x = c(xleft, xleft, xright, xright),
                             y = c(ybottom, ytop, ytop, ybottom))
                
                # Polygon
            }else{
                poly <-
                    DescTools::DrawRegPolygon(x = args$x,
                                              y = args$y,
                                              radius.x = args$radius.x,
                                              radius.y = args$radius.y,
                                              rot = deg2rad(args$angle),
                                              nv = 4,
                                              col = "transparent",
                                              plot = TRUE)
            }
            
            # User input
            check <- utils::menu(choices = c("yes", "no"),
                                 title = "Happy with this cropping area?")
            
        }
    }
    
    p = sp::Polygon(cbind(poly$x, poly$y))
    ps = sp::Polygons(list(p),1)
    sps = sp::SpatialPolygons(list(ps))
    
    # Crop and mask
    img <- raster::crop(img, raster::extent(sps))
    img <- raster::mask(img, sps)
    
    if (rtn_format == "matrix") {
        # Coerce to matrix
        img <- raster::as.matrix(img)
        # Flip
        img <-
            Thermimage::mirror.matrix(Thermimage::rotate180.matrix(img))
        return(img)
    }else{
        return(img)
    }
}

# Function to request multiple user inputs sequentially
readlines <- function(...) {
    lapply(list(...), readline)
}

# Function to convert degrees to radians
deg2rad <- function(deg) deg * (pi / 180)

define_params <- function(shape){
    
    if (shape != 2) {
        # Ask user for parameters
        new_args <- readlines("x coordinate of origin: ",
                              "y coordinate of origin: ",
                              "x-axis radius: ",
                              "y-axis radius: ",
                              "angle (in degrees): " )
    }else{
        # Ask user for parameters
        new_args <- readlines("x coordinate of origin: ",
                              "y coordinate of origin: ",
                              "x-axis radius: ",
                              "y-axis radius: ") 
    }
    
    # Coerce to numeric
    new_args <- lapply(new_args, function(x){
        tryCatch(
            {
                as.numeric(x)
            },
            warning = function(w) {
                message("Non-numeric value specified: ", x, "\n")
            })
    })
    return(new_args)
}






