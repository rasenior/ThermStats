#' kurtosis
#'
#' Helper function to calculate kurtosis, using
#' \code{moments::}\code{\link[moments]{kurtosis}}.
#' @param x Numeric vector or matrix.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @examples
#' kurtosis(1:100)
#' kurtosis(matrix(1:100))
#' @export

kurtosis <- function(x, na.rm = TRUE) {
  # Convert to numeric vector if matrix
  if(is.matrix(x)) x <- as.numeric(x)
  if(na.rm) x <- na.omit(x)
  return(moments::kurtosis(x))
}
