#' Raw FLIR E40 thermal image data and camera parameters
#'
#' The data was extracted from a FLIR E40 thermal image using
#' the package \pkg{Thermimage}. An alternative is
#' to use the freely availble software
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}.
#'
#' @docType data
#' @usage flir_raw
#' @format A list consisting of:
#'   \itemize{
#'     \item{raw_dat} {= A list of 120 x 160 numeric matrices of the raw 16-bit encoded
#'     values from four FLIR thermal images included in the package distribution.
#'     Extracted using \code{Thermimage::}\code{\link[Thermimage]{readflirJPG}}.}
#'     \item{camera_params} {= A dataframe of calibration constants unique to
#'     each FLIR camera.
#'     Extracted using \code{Thermimage::}\code{\link[Thermimage]{flirsettings}}.}
#'   }
#' @keywords datasets
#' @references \pkg{Thermimage}, available on
#' \href{https://cran.r-project.org/package=Thermimage}{CRAN} and
#' \href{https://github.com/gtatters/Thermimage}{GitHub}
#'
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}
#' @examples
#' hist(flir_raw$raw_dat$`8565`)
#' flir_raw$camera_params
"flir_raw"
