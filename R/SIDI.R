#' SIDI
#'
#' Helper function to calculate Simpson Diversity Index.
#' @param x Numeric vector or matrix.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @examples
#' SIDI(c(1:100, NA))
#' @keywords internal

SIDI <- function(x, na.rm = TRUE) {
  # Convert to numeric vector if matrix
  if(is.matrix(x)) x <- as.numeric(x)
  if(na.rm){x <- stats::na.omit(x)}

  # Count all occurrences of each unique temperature
  props <- table(x) / length(x)

  # Calculate Simpson diversity index
  result <- 1 - sum (props * props)
  return(result)
}
