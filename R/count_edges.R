#' count_edges
#'
#' Count the number of edges shared by cells of the same class within a matrix
#' @param in_dir Path to directory where thermal images are stored.
#' @param write_results Should the results be written as an Rdata file? Defaults to true.
#' @param out_dir Path to directory where output Rdata file will be stored. Defaults to working directory.
#' @return A list containing:
#'  \item{raw_dat}{A list with one element per input thermal image. Each element is a numeric matrix of the
#' raw infrared data.}
#'  \item{camera_params}{A dataframe of callibration constants unique to each camera.}
#' @examples
#' # Batch extract four FLIR thermal images included in this package.
#' results <- batch_extract(system.file("extdata", package = "PatchStatsFLIR"), write_results = FALSE)
#' @export
#'
count_edges <- function(mat, classes = NULL){

  # Define number of unique classes if they are not passed as an argument
  if(is.null(classes)){
    classes<-sort(unique(c(Matrix)))
  }

  # Define number of columns
  cols<-ncol(mat)
  rows<-nrow(mat)

  # Empty dataframe to populate
  results <-
    data.frame(class = classes, obs_edges = rep(0,length(classes)))


  # Moving horizontally (to the right) across matrix
  for(i in 1:(cols-1)){
    for(j in 1:rows){
      # Define focal value
      val_ij <- mat[j,i]

      # Define value of cell immediately to the right in the matrix
      nb_val<-mat[j,i+1]

      # If the neighbour has the same value, add 1 onto the counter of
      # the corresponding value
      # N.B. Can only test this if both of these positions are not NA
      if(!(is.na(val_ij)) & !(is.na(nb_val))){
        if(val_ij == nb_val){
          results[results[,"class"] == val_ij, "obs_edges"] <-
            results[results[,"class"] == val_ij,"obs_edges"] + 1
        }
      }
    }
  }

  # Moving vertically down mat
  for(i in 1:cols){
    for(j in 1:(rows-1)){
      # Define focal value
      val_ij <- mat[j,i]
      # Define value of cell immediately below in the mat
      nb_val <- mat[j+1,i]

      # If the neighbour has the same value, add 1 onto the counter of
      # the corresponding value

      # N.B. Can only test this if both of these positions are not NA
      if(!(is.na(val_ij)) & !(is.na(nb_val))){
        if(val_ij == nb_val){
          results[results[,"class"] == val_ij,"obs_edges"]<-
            results[results[,"class"] == val_ij,"obs_edges"]+1
        }
      }
    }
  }

  return(results)

}
