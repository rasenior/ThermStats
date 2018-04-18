
# mat <- patch_mat

set.seed(317)
cols = 1000
rows = 500
mat = matrix(sample(1:3, 500000, replace = TRUE), nrow = rows, ncol = cols)

new_fn <- function(mat, bidirectional = FALSE){

  # Define row and column length
  n_cols <- ncol(mat)
  n_rows <- nrow(mat)

  # Define row and column indices
  col_ind <- 2:(n_cols + 1)
  row_ind <- 2:(n_rows + 1)

  # Pad matrix with NAs
  mat.pad <- rbind(NA, cbind(NA, mat, NA), NA)

  # Create TRUE/FALSE neighbour matrix
  neigh <-
    cbind(N  = as.vector(mat.pad[row_ind - 1, col_ind    ] == mat.pad[row_ind, col_ind]),
          E  = as.vector(mat.pad[row_ind    , col_ind + 1] == mat.pad[row_ind, col_ind]),
          S  = as.vector(mat.pad[row_ind + 1, col_ind    ] == mat.pad[row_ind, col_ind]),
          W  = as.vector(mat.pad[row_ind    , col_ind - 1] == mat.pad[row_ind, col_ind]))

  # Sum matches by row
  edge_mat <- matrix(rowSums(neigh, na.rm = TRUE),
                      ncol = n_cols, nrow = n_rows)

  # Define unique classes
  classes <- sort(unique(c(mat)))

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


