#' Statistics from \code{stats_by_group}
#'
#' Results of running \code{stats_by_group} for FLIR photos included in the 
#' package installation, which can be located by:
#' \code{list.files(system.file("extdata/", package = "ThermStats"), full.names = TRUE)}.
#' @docType data
#' @usage flir_stats_group
#' @format A list consisting of:
#'   \itemize{
#'     \item{df} {= A dataframe with one row for each pixel, and variables denoting:
#'     the pixel value (val); the original spatial location of the pixel (x and y);
#'     its patch classification (G_bin) into a hot (1), cold (-1) or no patch (0)
#'     according to the Z value (see \code{spdep::}\code{\link[spdep]{localG}});
#'     the unique ID of the patch in which the pixel fell;
#'     and the matrix ID (if applicable).}
#'     \item{patches} {= A list of SpatialPolygonsDataFrames of hot and cold patches, 
#'     named according to \code{grouping_var}. Hot patches have a value of 1, and 
#'     cold patches a value of -1.}
#'     \item{pstats} {= A dataframe with patch statistics for hot patches and cold
#'     patches, respectively. See \code{\link{patch_stats}} for details of all the
#'     statistics returned.}
#'   }
#' @keywords datasets
#' @encoding UTF-8
#' @details This data is primarily included to speed up build time of the package
#' vignette (see \code{browseVignettes("ThermStats")}). You can reproduce 
#' \code{flir_stats_group} by following the code in the vignette.
#' @examples
#' head(flir_stats_group$df)
#' flir_stats_group$pstats
#' plot_patches(df = flir_stats_group$df,
#'              patches = flir_stats_group$patches, 
#'              save_plot = FALSE, 
#'              print_plot = TRUE,
#'              plot_distribution = FALSE)
"flir_stats_group"



