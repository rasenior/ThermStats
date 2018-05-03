#' perc_5
#'
#' Helper function to calculate 5th percentile.
#' @param x Numeric vector over which to calculate 5th percentile.
#' @param na.rm Whether to remove NAs. Defaults to TRUE.
#' @return A named vector.
#' @examples
#' perc_5(c(1:100, NA))
#' @export

perc_5 <- function(x, na.rm = TRUE) {
  quantile(x = x, probs = 0.05, na.rm = na.rm)
  }

