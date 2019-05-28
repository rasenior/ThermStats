#' perc_5
#'
#' Helper function to calculate 5th percentile.
#' @param x Numeric vector over which to calculate 5th percentile.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @keywords internal

perc_5 <- function(x, na.rm = TRUE) {
  return(stats::quantile(x = x, probs = 0.05, names = FALSE, na.rm = na.rm))
  }

