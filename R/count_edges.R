#' count_edges
#'
#' Count the number of edges shared by cells of the same class within a matrix
#' @param mat The matrix.
#' @param classes Manual specificiation of classes (optional). Use this to
#' exclude classes not of interest, such as background values.  If not
#' specified, all classes will be used. Classes must be numeric.
#' @param bidirectional Should both sides of each shared edge be counted?
#' Defaults to FALSE.
#' @param diagonals Should diagonal neighbours be included? Defaults to FALSE.
#' @return A matrix containing:
#'  \item{class}{The unique classes for which edges were counted.}
#'  \item{obs_edges}{The number of observed shared edges.}
#' @examples
#' # Create matrix
#' set.seed(317)
#' cols <- 100
#' rows <- 50
#' mat <- matrix(sample(1:3, cols * rows, replace = TRUE),
#'               nrow = rows, ncol = cols)
#'
#' # Count edges in each of the three classes (1, 2 or 3)
#' count_edges(mat)
#' @export
#'
count_edges <- function(mat, classes = NULL,
                        bidirectional = FALSE, diagonals = FALSE){

  # Define row and column length
  n_cols <- ncol(mat)
  n_rows <- nrow(mat)

  # Pad matrix with NAs
  mat.pad <- rbind(NA, cbind(NA, mat, NA), NA)

  # Define row and column indices (not including NAs)
  col_ind <- 2:(n_cols + 1)
  row_ind <- 2:(n_rows + 1)

  # Create neighbour matrix (TRUE if neighbour is same value)
  if(diagonals){
    # Including diagonal neighbours
    neigh <-
      cbind(N  = as.vector(mat.pad[row_ind - 1, col_ind    ] == mat.pad[row_ind, col_ind]),
            NE = as.vector(mat.pad[row_ind - 1, col_ind + 1] == mat.pad[row_ind, col_ind]),
            E  = as.vector(mat.pad[row_ind    , col_ind + 1] == mat.pad[row_ind, col_ind]),
            SE = as.vector(mat.pad[row_ind + 1, col_ind + 1] == mat.pad[row_ind, col_ind]),
            S  = as.vector(mat.pad[row_ind + 1, col_ind    ] == mat.pad[row_ind, col_ind]),
            SW = as.vector(mat.pad[row_ind + 1, col_ind - 1] == mat.pad[row_ind, col_ind]),
            W  = as.vector(mat.pad[row_ind    , col_ind - 1] == mat.pad[row_ind, col_ind]),
            NW = as.vector(mat.pad[row_ind + 1, col_ind - 1] == mat.pad[row_ind, col_ind]))
  }else{
    # Including only vertical and horizontal neighbours
    neigh <-
      cbind(N  = as.vector(mat.pad[row_ind - 1, col_ind    ] == mat.pad[row_ind, col_ind]),
            E  = as.vector(mat.pad[row_ind    , col_ind + 1] == mat.pad[row_ind, col_ind]),
            S  = as.vector(mat.pad[row_ind + 1, col_ind    ] == mat.pad[row_ind, col_ind]),
            W  = as.vector(mat.pad[row_ind    , col_ind - 1] == mat.pad[row_ind, col_ind]))
  }

  # Sum matches by row
  edge_mat <- matrix(rowSums(neigh, na.rm = TRUE),
                     ncol = n_cols, nrow = n_rows)

  # Define number of unique classes if they are not passed as an argument
  if(is.null(classes)){
    classes <- sort(unique(c(mat)))
  }

  # Sum shared edges by class
  results <- sapply(classes, function(x) sum(edge_mat[which(mat == x)]))
  results <- matrix(c(classes, results), ncol = 2)
  colnames(results) <- c("class", "obs_edges")

  # If only counting edges once, divide by two
  if(!(bidirectional)){
    results[,"obs_edges"] <- results[,"obs_edges"] / 2
  }

  return(results)

}
