#' perc_95
#'
#' Helper function to calculate 5th percentile.
#' @param x Numeric vector over which to calculate 95th percentile.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @keywords internal

perc_95 <- function(x, na.rm = TRUE) {
  return(stats::quantile(x = x, probs = 0.95, names = FALSE, na.rm = na.rm))
  }

