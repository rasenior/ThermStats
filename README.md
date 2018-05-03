# ThermStats

Calculate statistics for thermal images in R.

Designed primarily for FLIR thermal images, but some functions can also be applied to any temperature raster coerced to a matrix. For FLIR images, the following steps should be followed:

1. `batch_extract`is a batch implementation of the `readflirJPG` function in `Thermimage`. Requires only a directory of FLIR thermal images, and the external software [Exiftool](http://www.sno.phy.queensu.ca/~phil/exiftool/ "Exiftool"). Also retrieves camera-specific parameters for converting raw thermal data, using the function `flirsettings` in `Thermimage`.
2. `batch_convert` is a batch implementation of the `raw2temp` function in `Thermimage`. It uses environmental parameters defined by the user, and Planck constants specific to the camera (extracted in `batch_extract`).
3. `stats_by_group` calculates pixel and patch statistics across photos within a specified grouping. Ideal for sampling designs where multiple images are obtained per sampling event, and where the user wishes to summarise statistics across all photos within a single sampling event. The function will also work just as well with individual photos.

The functions called by `stats_by_group` can also be called directly. This is useful if the data are not derived from a FLIR camera, or the user wishes to process single matrices (e.g. for plotting):

1. `get_patches` identifies hot and cold spots in temperature matrices, using `localG` in the `spdep` package. Calculates patch statistics, such as the size, diversity and average value of hot and cold spots. Also calculates the number of observed shared edges versus maximum number of shared edges, across pixels within hot and cold spots. This can be used to calculate Aggregation Index (number of observed shared edges / maximum number of shared edges), or used within a Generalized Linear Modelling framework (number of observed shared edges versus number of non-shared edges, with binomial error distribution).
2. `count_edges` is called by `get_patches` to count the number of shared edges for unique classes in a matrix. Can be used for any numeric matrix.
3. `perc_5` is a helper function for `stats_by_group` to calculate 5th percentile. Can be used for any numeric vector.
4. `perc_95` is a helper function for `stats_by_group` to calculate 95th percentile. Can be used for any numeric vector.
5. `SHDI` is a helper function for `stats_by_group` to calculate Shannon Diversity Index. Can be used for any numeric vector.
6. `SIDI` is a helper function for `stats_by_group` to calculate Simpson Diversity Index. Can be used for any numeric vector.

Patches can also be plotted:

1. Use `get_patches` with `return_vals = c("df","patches")` to return a dataframe of raw data and patch polygons for a single matrix.
1. Use `plot_patches` to plot:
  1. Temperature distribution  (if `plot_distribution = TRUE`).
  2. Raw temperature raster overlaid with outlines of hot and cold spots. 

  
 Example data are included for demonstration (`flir11835`). The temperature matrix itself can be easily extracted from a FLIR jpeg using the functions `readflirJPG` and `raw2temp` from the package [`Thermimage`](https://CRAN.R-project.org/package=Thermimage). An alternative is to use the freely available software ['FLIR Tools'](http://www.flir.com/instruments/display/?id=54865). 
