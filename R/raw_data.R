#' FLIR E40 thermal image data
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
#'     \item{flir_matrix} {= A 120 x 160 numeric temperature matrix.}
#'     \item{photo_no} {= The photo number.}
#'   }
#' @keywords datasets
#' @references
#' \itemize{
#' \item \pkg{Thermimage} is available on
#'   \href{https://cran.r-project.org/package=Thermimage}{CRAN} and
#'   \href{https://github.com/gtatters/Thermimage}{GitHub}.
#' \item FLIR Tools is available at \url{http://www.flir.com/instruments/display/?id=54865}.
#' }
#' @examples
#' hist(flir_raw$raw_dat$`8565`)
"flir_raw"
