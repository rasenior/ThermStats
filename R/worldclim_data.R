#' WorldClim2 mean monthly temperature data for Sulawesi
#'
#' Gridded temperature data at 30 arc-second resolution for the island of
#' Sulawesi. Temperature is average January temperature (°C) for the 'current'
#' time period (1970-2000).
#'
#' @docType data
#' @usage sulawesi_temp
#' @format A geographic temperature raster
#' @keywords datasets
#' @references Fick, S.E. and Hijmans, R.J. (2017),
#' Worldclim 2: New 1-km spatial resolution climate surfaces for global land
#' areas. International Journal of Climatology. Available at:
#' \url{http://worldclim.org/version2}.
#' @examples
#' raster::plot(sulawesi_temp)
#' hist(sulawesi_temp)
"sulawesi_temp"
