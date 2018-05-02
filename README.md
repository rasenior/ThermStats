# ThermStats

Calculate statistics for thermal images in R.

Designed primarily for FLIR thermal images, but some functions can also be applied to any temperature raster, in matrix format.

`batch_extract`is the first step for FLIR thermal images. Pass a directory of FLIR thermal images, and this function will batch implement the function `readFLIRjpeg

`get_patches` finds hot and cold spots in temperature matrices and calculates their patch statistics.

`plot_patches` takes results from `get_patches` and plots:
  1. Temperature distribution.
  2. Raw temperature raster overlaid with outlines of hot and cold spots.
  
 Example data are included for demonstration (`flir11835`). The temperature matrix itself can be easily extracted from a FLIR jpeg using the functions `readflirJPG` and `raw2temp` from the package [`Thermimage`](https://CRAN.R-project.org/package=Thermimage). An alternative is to use the freely available software ['FLIR Tools'](http://www.flir.com/instruments/display/?id=54865). 
