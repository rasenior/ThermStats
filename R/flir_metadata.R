#' Metadata associated with FLIR E40 thermal images
#'
#' Metadata with environmental parameters necessary to accurately convert the
#' raw data of FLIR thermal images into temperature values.
#'
#' @docType data
#' @usage flir_metadata
#' @format A dataframe of environmental parameters with one row for each photo.
#' The photo number corresponds to the element name in \code{\link{flir_raw}$raw_dat}.
#' @keywords datasets
#' @examples
#' flir_metadata
"flir_metadata"
