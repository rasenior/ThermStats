#' FLIR E40 thermal image data
#'
#' The data was extracted from a FLIR E40 thermal image using
#' the package \code{Thermimage}. An alternative is
#' to use the freely availble software 'FLIR Tools'
#' (\url{http://www.flir.com/instruments/display/?id=54865}).
#'
#' @docType data
#' @usage flir_raw
#' @format A list consisting of:
#'   \itemize{
#'     \item{flir_matrix} {= A 120 x 160 numeric temperature matrix.}
#'     \item{photo_no} {= The photo number.}
#'   }
#' @keywords datasets
#' @references \code{Thermimage}, available on CRAN
#'   (\url{https://cran.r-project.org/package=Thermimage}) and GitHub
#'   (\url{https://github.com/gtatters/Thermimage}).
#'
#'   FLIR Tools (\url{http://www.flir.com/instruments/display/?id=54865}).
#' @examples
#' hist(flir_raw$raw_list$`8565`)
"flir_raw"
