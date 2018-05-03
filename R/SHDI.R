#' SHDI
#'
#' Helper function to calculate Shannon Diversity Index.
#' @param x Numeric vector.
#' @param na.rm Whether to remove NAs. Defaults to TRUE.
#' @return A named vector.
#' @examples
#' SHDI(c(1:100, NA))
#' @export

SHDI <- function(x, na.rm = TRUE) {
  if(na.rm){x <- na.omit(x)}
  # Identify unique pixels
  unique_temp <- unique(x)
  # Calculate the proportion of pixels in each category
  props <-
    sapply(unique_temp, function(temp){
      length(which(x == temp)) / length(x)
    })

  # Calculate Shannon diversity index
  result <- -sum(props * log(props))
  return(result)
}
