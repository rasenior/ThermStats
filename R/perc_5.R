#' perc_5
#'
#' Helper function to calculate 5th percentile.
#' @param x Numeric vector over which to calculate 5th percentile.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @examples
#' perc_5(c(1:100, NA))
#' @keywords internal

perc_5 <- function(x, na.rm = TRUE) {
  return(quantile(x = x, probs = 0.05, names = FALSE, na.rm = na.rm))
  }

