#' Temperature matrix from thermal image of wasp nest
#'
#' The data was extracted from a FLIR SC660 thermal image of a wasp nest,
#' using the package \pkg{Thermimage}. An alternative is
#' to use the freely availble software
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}, with
#' which this image is freely distributed.
#'
#' @docType data
#' @usage waspnest_mat
#' @format A 640 x 480 numeric temperature matrix.
#' @keywords datasets
#' @references \pkg{Thermimage}, available on
#' \href{https://cran.r-project.org/package=Thermimage}{CRAN} and
#' \href{https://github.com/gtatters/Thermimage}{GitHub}
#'
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}
#' @examples
#' mean(waspnest_mat)
#' hist(waspnest_mat)
#' # Convert to raster
#' waspnest_raster <- raster::raster(waspnest_mat)
#' # Visualise using the rasterVis package
#' rasterVis::levelplot(waspnest_raster,
#'                      layers = 1,
#'                      margin = list(FUN = 'median'),
#'                      contour= FALSE)
"waspnest_mat"
