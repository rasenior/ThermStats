#' SIDI
#'
#' Helper function to calculate Simpson Diversity Index.
#' @param x Numeric vector or matrix.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @examples
#' SIDI(c(1:100, NA))
#' @export

SIDI <- function(x, na.rm = TRUE) {
  # Convert to numeric vector if matrix
  if(is.matrix(x)) x <- as.numeric(x)
  if(na.rm){x <- na.omit(x)}

  # Identify unique pixels
  unique_temp <- unique(x)
  # Calculate the proportion of pixels in each category
  props <-
    sapply(unique_temp, function(temp){
      length(which(x == temp)) / length(x)
    })

  # Calculate Simpson diversity index
  result <- 1 - sum (props * props)
  return(result)
}
