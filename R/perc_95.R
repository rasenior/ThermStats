#' perc_95
#'
#' Helper function to calculate 95th percentile.
#' @param x Numeric vector over which to calculate 95th percentile.
#' @param na.rm Whether to remove NAs. Defaults to TRUE.
#' @return A named vector.
#' @examples
#' perc_95(c(1:100, NA))
#' @export

perc_95 <- function(x, na.rm = TRUE) {
  quantile(x = x, probs = 0.95, na.rm = na.rm)
}
