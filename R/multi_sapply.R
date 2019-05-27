#' multi_sapply
#'
#' Apply multiple functions to a numeric vector.
#' @param ... First argument should be the vector, all subsequent arguments
#' are functions to apply (naming functions is optional).
#' @return A wide dataframe with a column for the results of each function.
#' @details NA values are always removed.
#' @references Code snippet by \href{http://bogumilkaminski.pl/about/}{Bogumił Kamiński},
#' adapted from:
#' \url{http://rsnippets.blogspot.co.uk/2011/11/applying-multiple-functions-to-data.html}
#' @examples
#' multi_sapply(1:100, mean, max, min)
#' multi_sapply(c(1:100, NA), mean, max, min)
#' multi_sapply(1:100, Mean = mean, Max = max, Min = min)
#' @keywords internal
#'
# Function to apply multiple functions to a vector
multi_sapply <- function(...) {
  # Reads in all arguments passed to function, including data
  arglist <- match.call(expand.dots = FALSE)$...

  # Deparse argument names
  var.names <- sapply(arglist, deparse)

  # For all arguments that had function names specified,
  # substitue name from deparsed expression by the given name
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]

  # Evaluate the expressions given in arguments;
  # go two generations back as we apply eval.parent
  # within lapply function
  arglist <- lapply(arglist, eval.parent, n = 2)

  # First argument contains data set - assign & then remove from the list
  dat <- arglist[[1]]
  arglist[[1]] <- NULL

  # Apply every function
  result <-
    sapply(arglist, function(fn) fn(dat, na.rm = TRUE))

  # Coerce to dataframe
  result <- as.data.frame(t(result))

  # In defining column names remove first element as it was name of dataset
  colnames(result) <- var.names[-1]
  return(result)
}
