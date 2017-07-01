# PatchStatsFLIR
Tools to analyse FLIR thermal images in R

`get_patches` finds hot and cold spots in temperature matrices and calculates their patch statistics.

`plot_patches` takes results from `get_patches` and plots:
  1. Temperature distribution.
  2. Raw temperature raster overlaid with outlines of hot and cold spots.
  
 Example data are included for demonstration (`flir11835`). The temperature matrix itself can be easily extracted from a FLIR jpeg using the functions `readflirJPG` and `raw2temp` from the package [`Thermimage`](https://CRAN.R-project.org/package=Thermimage). An alternative is to use the freely available software ['FLIR Tools'](http://www.flir.com/instruments/display/?id=54865). 
