#' patch_stats
#'
#' Calculate patch statistics for a given class (i.e. cell value) in a numeric matrix.
#' @param val_mat A numeric matrix.
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
#'  divided by the minimum perimeter.
#'  See \href{http://www.umass.edu/landeco/research/fragstats/documents/fragstats.help.4.2.pdf}{FRAGSTATS} p. 101.}
#'  \item{AI}{Aggregation index. Calculated as the observed number of shared,
#'  inside edges divided by the maximum number that could be shared.
#'  See \href{https://doi.org/10.1023/A:1008102521322}{He et al. 2000}.}
#'  \item{cohesion}{Cohesion index. Calculated from the area and perimeter of
#'  cells in the given class relative to the area of the entire landscape.
#'  See \href{https://doi.org/10.2307/2265590}{Scuhmaker 1996}.}
#' @details Statistics are based on and inspired by
#' \code{SDMTools::}\code{\link[SDMTools]{ClassStat}},
#' \href{http://www.umass.edu/landeco/research/fragstats/fragstats.html}{FRAGSTATS}
#' and \href{https://doi.org/10.1111/2041-210X.12488}{Faye et al. 2016}
#' @references
#' \itemize{
#' \item \pkg{SDMTools}: Species Distribution Modelling Tools: Tools for
#' processing data associated with species distribution modelling exercises.
#' Available on \href{https://CRAN.R-project.org/package=SDMTools}{CRAN}.
#' \item McGarigal, K., S. A. Cushman, M. C. Neel, and E. Ene. 2002.
#' FRAGSTATS: Spatial Pattern Analysis Program for Categorical Maps. Computer
#' software program produced by the authors at the University of Massachusetts,
#' Amherst. Available at the following web site:
#' \url{www.umass.edu/landeco/research/fragstats/fragstats.html}
#' \item Faye, E. , Rebaudo, F. , Yánez‐Cajo, D. , Cauvy‐Fraunié, S. ,
#' Dangles, O. and Tatem, A. (2016), A toolbox for studying thermal
#' heterogeneity across spatial scales: from unmanned aerial vehicle
#' imagery to landscape metrics. Methods Ecol Evol, 7: 437-446.
#' \url{https://doi.org/10.1111/2041-210X.12488}
#' \item He, H.S., DeZonia, B.E. & Mladenoff, D.J. (2000),
#' An aggregation index (AI) to quantify spatial patterns of landscapes.
#' Landscape Ecology, 15: 591.
#' \url{https://doi.org/10.1023/A:1008102521322}
#' \item Schumaker, N. H. (1996),
#' Using landscape indices to predict habitat connectivity.
#' Ecology, 77: 1210-1225.
#' \url{https://doi.org/10.2307/2265590}
#' }
#' @examples
#' # Create matrix
#' matrix(c(1,1,2,1,
#'          1,0,2,0,
#'          1,2,0,1,
#'          2,2,0,1), nrow = 4, ncol  = 4,
#'          byrow = TRUE)
#'
#' # Count edges in each of the three classes (1, 2 or 3)
#' count_edges(val_mat)
#' @export
#'

patch_stats <- function(val_mat, class){

  # Calculate area (total number of cells)
  area <- length(which(val_mat == class))

  # Calculate the total area of the matrix
  total_area <- length(val_mat)

  # Calculate 'n', the side of the largest integer square smaller than 'area'
  n <- floor(sqrt(area))
  m <- area - (n)^2

  # Calculate the maximum number of edges that could be shared, given the area
  max_edge <- max_edges(n = n, m = m)

  # Calculate the actual number of edges that are shared
  shared_edge <- count_edges(val_mat = val_mat, class = class)

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
               total_area = total_area,
               prop = area / total_area,
               perim = perim,
               perim_min = min_perim,
               inside_edges = shared_edge,
               inside_non_edge = max_edge - shared_edge,
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
