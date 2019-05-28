#' count_edges
#'
#' Count the number of edges shared by cells of the same class within a matrix
#' @param mat A numeric matrix.
#' @param classes The patch class of interest. Must be numeric.
#' @param bidirectional Should both sides of each shared edge be counted?
#' Defaults to FALSE.
#' @param diagonals Should diagonal neighbours be included? Defaults to FALSE.
#' @return A numeric value for the number of edges shared by cells of the given
#' class.
#' @keywords internal
#' @examples
#' # Create matrix
#' set.seed(317)
#' cols <- 100
#' rows <- 50
#' mat <- matrix(sample(1:3, cols * rows, replace = TRUE),
#'               nrow = rows, ncol = cols)
#'
#' # Count edges in each of the three classes (1, 2 or 3)
#' results <- lapply(1:3, count_edges, mat = mat)
#' do.call("rbind", results)
#' [,1]
#' [1,] 1051
#' [2,] 1179
#' [3,] 1028
#' 
#' @export

count_edges <- function(mat, class,
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

  # Sum shared edges by class
  shared_edge <-  sum(edge_mat[which(mat == class)])

  # If only counting edges once, divide by two
  if(!(bidirectional)){
    shared_edge <- shared_edge / 2
  }

  return(shared_edge)

}
