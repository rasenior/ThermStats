# PatchStatsFLIR
Tools to analyse FLIR thermal images in R

2. `batch_convert` is a batch implementation of the `raw2temp` function in `Thermimage`. It uses environmental parameters defined by the user, and Planck constants specific to the camera (extracted in `batch_extract`).
3. `stats_by_group` calculates pixel and patch statistics across photos within a specified grouping. Ideal for sampling designs where multiple images are obtained per sampling event, and where the user wishes to summarise statistics across all photos within a single sampling event. The function will, however, work just as well with individual photos.

The functions called by `stats_by_group` can also be called directly, if preferred:

1. `get_patches` identifies hot and cold spots in temperature matrices, using `localG` in the `spdep` package. Calculates patch statistics, such as the size, diversity and average value of hot and cold spots. Also calculates the number of observed shared edges versus maximum number of shared edges, across pixels within hot and cold spots. This can be used to calculate Aggregation Index (number of observed shared edges / maximum number of shared edges), or used within a Generalized Linear Modelling framework (number of observed shared edges versus number of non-shared edges, with binomial error distribution).

2. `plot_patches` takes results from `get_patches` and plots:
  1. Temperature distribution.
  2. Raw temperature raster overlaid with outlines of hot and cold spots.
  
3. `count_edges` is called by `get_patches` to count the number of shared edges for unique classes in a matrix. Can be used for any numeric matrix.
  
 Example data are included for demonstration (`flir11835`). The temperature matrix itself can be easily extracted from a FLIR jpeg using the functions `readflirJPG` and `raw2temp` from the package [`Thermimage`](https://CRAN.R-project.org/package=Thermimage). An alternative is to use the freely available software ['FLIR Tools'](http://www.flir.com/instruments/display/?id=54865). 
