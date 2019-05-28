#' kurtosis
#'
#' Helper function to calculate kurtosis, using
#' \code{moments::}\code{\link[moments]{kurtosis}}.
#' @param x Numeric vector or matrix.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @keywords internal

kurtosis <- function(x, na.rm = TRUE) {
    # Convert to numeric vector if matrix
    if (is.matrix(x)) x <- as.numeric(x)
    if (na.rm) x <- stats::na.omit(x)
    
    if (requireNamespace("moments", quietly = TRUE)) {
        return(moments::kurtosis(x))
    }else {
        stop("Requires package 'moments'")
    }
}
