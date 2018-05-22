#' count_edges
#'
#' Count the number of edges shared by cells of the same class within a matrix
#' @param val_mat The matrix.
#' @param classes The patch class of interest. Must be numeric.
#' @param bidirectional Should both sides of each shared edge be counted?
#' Defaults to FALSE.
#' @param diagonals Should diagonal neighbours be included? Defaults to FALSE.
#' @return A numeric value for the number of edges shared by cells of the given
#' class.
#' @examples
#' # Create matrix
#' set.seed(317)
#' cols <- 100
#' rows <- 50
#' val_mat <- matrix(sample(1:3, cols * rows, replace = TRUE),
#'               nrow = rows, ncol = cols)
#'
#' # Count edges in each of the three classes (1, 2 or 3)
#' results <- lapply(1:3, count_edges, val_mat = val_mat)
#' results <- do.call("rbind", results)
#' @export

count_edges <- function(val_mat, class,
                        bidirectional = FALSE, diagonals = FALSE){

  # Define row and column length
  n_cols <- ncol(val_mat)
  n_rows <- nrow(val_mat)

  # Pad matrix with NAs
  val_mat.pad <- rbind(NA, cbind(NA, val_mat, NA), NA)

  # Define row and column indices (not including NAs)
  col_ind <- 2:(n_cols + 1)
  row_ind <- 2:(n_rows + 1)

  # Create neighbour matrix (TRUE if neighbour is same value)
  if(diagonals){
    # Including diagonal neighbours
    neigh <-
      cbind(N  = as.vector(val_mat.pad[row_ind - 1, col_ind    ] == val_mat.pad[row_ind, col_ind]),
            NE = as.vector(val_mat.pad[row_ind - 1, col_ind + 1] == val_mat.pad[row_ind, col_ind]),
            E  = as.vector(val_mat.pad[row_ind    , col_ind + 1] == val_mat.pad[row_ind, col_ind]),
            SE = as.vector(val_mat.pad[row_ind + 1, col_ind + 1] == val_mat.pad[row_ind, col_ind]),
            S  = as.vector(val_mat.pad[row_ind + 1, col_ind    ] == val_mat.pad[row_ind, col_ind]),
            SW = as.vector(val_mat.pad[row_ind + 1, col_ind - 1] == val_mat.pad[row_ind, col_ind]),
            W  = as.vector(val_mat.pad[row_ind    , col_ind - 1] == val_mat.pad[row_ind, col_ind]),
            NW = as.vector(val_mat.pad[row_ind + 1, col_ind - 1] == val_mat.pad[row_ind, col_ind]))
  }else{
    # Including only vertical and horizontal neighbours
    neigh <-
      cbind(N  = as.vector(val_mat.pad[row_ind - 1, col_ind    ] == val_mat.pad[row_ind, col_ind]),
            E  = as.vector(val_mat.pad[row_ind    , col_ind + 1] == val_mat.pad[row_ind, col_ind]),
            S  = as.vector(val_mat.pad[row_ind + 1, col_ind    ] == val_mat.pad[row_ind, col_ind]),
            W  = as.vector(val_mat.pad[row_ind    , col_ind - 1] == val_mat.pad[row_ind, col_ind]))
  }

  # Sum matches by row
  edge_mat <- matrix(rowSums(neigh, na.rm = TRUE),
                     ncol = n_cols, nrow = n_rows)

  # Sum shared edges by class
  shared_edge <-  sum(edge_mat[which(val_mat == class)])

  # If only counting edges once, divide by two
  if(!(bidirectional)){
    shared_edge <- shared_edge / 2
  }

  return(shared_edge)

}
