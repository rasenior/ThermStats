#' FLIR E40 thermal image data
#'
#' The data was extracted from a FLIR E40 thermal image using
#' the package \pkg{Thermimage}. An alternative is
#' to use the freely availble software
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}.
#'
#' @docType data
#' @usage flir11835
#' @format A list consisting of:
#'   \itemize{
#'     \item{flir_matrix} {= A 120 x 160 numeric temperature matrix.}
#'     \item{photo_no} {= The photo number.}
#'   }
#' @keywords datasets
#' @references \pkg{Thermimage}, available on
#' \href{https://cran.r-project.org/package=Thermimage}{CRAN} and
#' \href{https://github.com/gtatters/Thermimage}{GitHub}
#'
#' \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools}
#' @examples
#' mean(flir11835$flir_matrix)
#' hist(flir11835$flir_matrix)
"flir11835"
