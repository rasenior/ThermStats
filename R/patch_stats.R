#' patch_stats
#'
#' Calculate patch statistics for patch classes of interest in a numeric matrix.
#' @param mat The matrix.
#' @param class The patch class for which statistics should be calculated.
#' @return A dataframe containing:
#'  \item{class}{The patch class for which statistics were calculated.}
#'  \item{area}{Total area (number of cells).}
#'  \item{prop}{Proportion of landscape area represented by cells in the given class.}
#'  \item{perim}{Total perimeter (number of outside edges).}
#'  \item{perim_min}{Minimum perimeter, given the number of cells.}
#'  \item{inside_edges}{Total number of shared, inside edges.}
#'  \item{inside_edges_max}{Maximum number of inside edges that could be shared,
#'  given the number of cells.}
#'  \item{shape_index}{Patch shape index. Calculated as the observed perimeter
#'  divided by the minimum perimeter.}
#'  \item{AI}{Aggregation index. Calculated as the observed number of shared,
#'  inside edges divided by the maximum number that could be shared. See He et al.}
#'  \item{cohesion}{Cohesion index. Calculated from the area and perimeter of
#'  cells in the given class relative to the area of the entire landscape.
#'  See Scuhmaker 1996.}
#'  \details{Inspiration taken from [\code{SDMTools}][1], FRAGSTATS and Faye et al. 2016
#'  \references{[1]: \url{https://cran.r-project.org/web/packages/SDMTools/index.html} "SDMTools"}
#' @examples
#' # Create matrix
#' matrix(c(1,1,2,1,
#'          1,0,2,0,
#'          1,2,0,1,
#'          2,2,0,1), nrow = 4, ncol  = 4,
#'          byrow = TRUE)
#'
#' # Count edges in each of the three classes (1, 2 or 3)
#' count_edges(mat)
#' @export
#'

patch_stats <- function(mat, class){

  # Calculate area (total number of cells)
  area <- length(which(mat == class))

  # Calculate the total area of the matrix
  total_area <- length(mat)

  # Calculate 'n', the side of the largest integer square smaller than 'area'
  n <- floor(sqrt(area))
  m <- area - (n)^2

  # Calculate the maximum number of edges that could be shared, given the area
  max_edge <- max_edges(n = n, m = m)

  # Calculate the actual number of edges that are shared
  shared_edge <- count_edges(mat = mat, class = class)

  # Calculate the minimum perimeter, given the area
  min_perim <- min_p(area = area, n = n, m = m)

  # Calculate the actual perimeter
  perim <- perimeter(area = area, shared_edge = shared_edge)

  # Calculate the shape index
  shape_index <- perim / min_perim

  # Calculate the aggregation index
  aggregation_index <- shared_edge / max_edge

  # Calculate the cohesion index (equation from Schumaker 1996)
  cohesion_index <-
    cohesion(area = area, perimeter = perim, total_area = total_area)

  # Create results dataframe
  results <-
    data.frame(class = class,
               area = area,
               prop = area / total_area,
               perim = perim,
               perim_min = min_perim,
               inside_edges = shared_edge,
               inside_edges_max = max_edge,
               shape_index = shape_index,
               aggregation = aggregation_index,
               cohesion = cohesion_index)
  return(results)
}

# Helper function to calculate the maximum number of edges that could be shared
# by cells in the given class, given their area in the landscape
max_edges <- function(n, m) {
  if (m == 0) {
    result <- 2 * n * (n - 1)
  } else if (m <= n) {
    result <- 2 * n * (n - 1) + (2 * m) - 1
  } else if (m > n) {
    result <- 2 * n * (n - 1) + (2 * m) - 2
  }
  return(result)
}

# Helper function to calculate the minimum perimeter of cells in the given
# class, given their area in the landscape
min_p <- function(area, n, m){
  if(m == 0){
    result <- 4 * n
  }else if((n^2 < area) & (area <= (n * (1+n)))){
    result <- (4 * n) + 2
  }else if(area > (n * (1+ n))){
    result <- (4 * n) + 4
  }
  return(result)
}

# Helper function to calculate the actual perimeter of cells in the given class,
# given their total area and the number of edges that they share
perimeter <- function(area, shared_edge){
  return((4 * area) - (2 * shared_edge))
}

# Helper function to calculate cohesion of cells in the given class
cohesion <- function(area, perimeter, total_area){
  cohesion <-
    (1 - (perimeter / (perimeter * sqrt(area)))) *
    (1 - (1 / sqrt(total_area)))^-1

  return(cohesion)
}
