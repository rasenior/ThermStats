#' perc_95
#'
#' Helper function to calculate 5th percentile.
#' @param x Numeric vector over which to calculate 95th percentile.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @examples
#' perc_95(c(1:100, NA))
#' @export

perc_95 <- function(x, na.rm = TRUE) {
  return(quantile(x = x, probs = 0.95, names = FALSE, na.rm = na.rm))
  }

