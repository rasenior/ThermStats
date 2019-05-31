













<img src="schematic.png" alt="Figure 1: Schematic summarising the key functions for processing groups of images (left) or a single image (right)." width="400px" />
<p class="caption">
Figure 1: Schematic summarising the key functions for processing groups
of images (left) or a single image (right).
</p>

Summary
=======

`ThermStats` is designed for biologists using thermography to quantify
thermal heterogeneity. It uses the `Thermimage` package (Tattersall,
[2019](#ref-tattersall_thermimage:_2017)) to batch process data from
FLIR thermal cameras, and takes inspiration from FRAGSTATS (McGarigal et
al., [2012](#ref-fragstats_2012)), `SDMTools` (VanDerWal et al.,
[2014](#ref-sdmtools_2014)), Faye et al.
([2016](#ref-faye_toolbox_2016)) and Shi et al.
([2016](#ref-shi_framework_2016)) to facilitate the calculation of
various metrics of thermal heterogeneity for any gridded temperature
data.

The package is available to download from GitHub using `devtools`:

    devtools::install_github("rasenior/ThermStats")

    library(ThermStats)

Once loaded, the code below can be followed step-by-step.

Extracting raw data
===================

Data are extracted from FLIR images using `batch_extract`. This is a
batch implementation of the `readflirJPG` function from `Thermimage`. It
requires only the path to the directory of FLIR thermal images, and the
freely available external software
[‘ExifTool’](https://www.sno.phy.queensu.ca/~phil/exiftool/ "ExifTool").
Besides raw data, this step also retrieves camera-specific calibration
parameters which are required later to convert raw data to temperature
values.

    # Batch extract thermal images included in ThermStats installation
    flir_raw <-
        batch_extract(in_dir = system.file("extdata", 
                                           package = "ThermStats"),
                      write_results = FALSE)

Converting raw data to temperature
==================================

Raw data are encoded in each thermal image as a 16 bit analog-to-digital
signal, which represents the radiance received by the infrared sensor.
The function `batch_convert` converts these raw data to temperature
values using equations from infrared thermography, via a batch
implementation of the function `raw2temp` in `Thermimage`. It uses the
calibration constants extracted in `batch_extract` and environmental
parameters defined by the user:

-   Emissivity = the amount of radiation emitted by a particular object,
    for a given temperature.
-   Object distance = the distance between the camera and the object of
    interest.
-   Reflected apparent temperature = thermal radiation that originates
    from other objects and is reflected by the object of interest.
-   Atmospheric temperature = the temperature of the atmosphere.
-   Relative humidity = the relative humidity of the atmosphere.

<!-- -->

    # Define raw data
    raw_dat <- flir_raw$raw_dat
    # Define camera calibration constants dataframe
    camera_params <- flir_raw$camera_params
    # Define metadata
    metadata <- flir_metadata
    # Create vector denoting the position of each photo within metadata
    photo_index <- match(names(raw_dat), 
                         metadata$photo_no)
    # Batch convert
    flir_converted <-
        batch_convert(
            raw_dat = raw_dat,
            # Emissivity = mean of range in Scheffers et al. 2017
            E = mean(c(0.982,0.99)),
            # Object distance = hypotenuse of right triangle where 
            # vertical side is 1.3 m (breast height) & angle down is 45°
            OD = (sqrt(2))*1.3,
            # Apparent reflected temperature & atmospheric temperature =
            # atmospheric temperature measured in the field
            RTemp = metadata$atm_temp[photo_index],
            ATemp = metadata$atm_temp[photo_index],
            # Relative humidity = relative humidity measured in the field
            RH = metadata$rel_humidity[photo_index],
            # Calibration constants from 'batch_extract'
            PR1 = camera_params[,"PlanckR1"],
            PB = camera_params[,"PlanckB"],
            PF = camera_params[,"PlanckF"],
            PO = camera_params[,"PlanckO"],
            PR2 = camera_params[,"PlanckR2"],
            # Whether to write results or just return
            write_results = FALSE)

Calculating thermal statistics
==============================

Statistics can be calculated for individual thermal images (in a matrix
or raster format), or across multiple images within a specified
grouping. The latter is useful for sampling designs where multiple
images are collected at each sampling event to capture temperature
across a wider sampling unit, such as a plot. In either case, statistics
can include summary statistics specified by the user – for example,
mean, minimum and maximum – as well as thermal connectivity (based on
the climate connectivity measure of McGuire et al.,
[2016](#ref-mcguire_achieving_2016)) and spatial statistics for hot and
cold spots, identified using the G\* variant of the Getis-Ord local
statistic (Getis and Ord, [1996](#ref-getis_local_1996)).

For an individual image, `get_stats` requires the user to specify the
image and the desired statistics. Statistics can be calculated for
geographic temperature data, in which case the user should also define
the extent and projection of the data.

    flir_stats <-
        get_stats( 
            # The temperature dataset
            img = flir_converted$`8565`,
            # The ID of the dataset
            id = "8565",
            # Whether or not to calculate thermal connectivity
            calc_connectivity = FALSE,
            # Whether or not to identify hot and cold spots
            patches = TRUE,  
            # The image projection (only relevant for geographic data)
            img_proj = NULL,
            # The image extent (only relevant for geographic data)
            img_extent = NULL, 
            # The data to return
            return_vals = c("df", # Temperature data as dataframe
                            "patches", # Patch outlines
                            "pstats"), # Patch statistics dataframe
            # The summary statistics of interest
            sum_stats = c("median", "SHDI",
                          "perc_5", "perc_95"))

For grouped images, `stats_by_group` requires the user to supply a list
of matrices or a raster stack, and (optionally) the metadata and the
name of the variable in the metadata that defines the grouping. Table 1
shows the metadata used in the code snippet, where photo number
(‘photo\_no’) defines individual temperature matrices, and the replicate
identity (‘rep\_id’) defines the grouping of photos. There are two
replicates, ‘T7P1’ and ‘T7P2’, and each has two associated photos.

<table>
<caption>Table 1: Example metadata denoting the grouping (‘rep_id’) of different thermal images. Statistics can be calculated over multiple images within a group, using the function <code>stats_by_group</code>.</caption>
<thead>
<tr class="header">
<th style="text-align: right;">photo_no</th>
<th style="text-align: left;">rep_id</th>
<th style="text-align: right;">atm_temp</th>
<th style="text-align: right;">rel_humidity</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">8565</td>
<td style="text-align: left;">T7P1</td>
<td style="text-align: right;">24.00</td>
<td style="text-align: right;">96</td>
</tr>
<tr class="even">
<td style="text-align: right;">8583</td>
<td style="text-align: left;">T7P1</td>
<td style="text-align: right;">24.00</td>
<td style="text-align: right;">96</td>
</tr>
<tr class="odd">
<td style="text-align: right;">8589</td>
<td style="text-align: left;">T7P2</td>
<td style="text-align: right;">23.25</td>
<td style="text-align: right;">98</td>
</tr>
<tr class="even">
<td style="text-align: right;">8613</td>
<td style="text-align: left;">T7P2</td>
<td style="text-align: right;">23.50</td>
<td style="text-align: right;">96</td>
</tr>
</tbody>
</table>

By default, both `get_stats` and `stats_by_group` return a dataframe
with patch statistics (Table 2) for each image or group, respectively.

<table>
<caption>Table 2: A snippet of hot spot patch statistics returned by <code>stats_by_group</code>, which implements <code>get_stats</code> within groups.</caption>
<thead>
<tr class="header">
<th style="text-align: right;">img_median</th>
<th style="text-align: right;">img_perc_5</th>
<th style="text-align: right;">img_perc_95</th>
<th style="text-align: right;">img_SHDI</th>
<th style="text-align: right;">hot_shape_index</th>
<th style="text-align: right;">hot_aggregation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">23.5</td>
<td style="text-align: right;">23</td>
<td style="text-align: right;">24.5</td>
<td style="text-align: right;">1.16</td>
<td style="text-align: right;">7.54</td>
<td style="text-align: right;">0.895</td>
</tr>
<tr class="even">
<td style="text-align: right;">24.0</td>
<td style="text-align: right;">23</td>
<td style="text-align: right;">25.0</td>
<td style="text-align: right;">1.68</td>
<td style="text-align: right;">7.80</td>
<td style="text-align: right;">0.855</td>
</tr>
</tbody>
</table>

Plotting
========

In addition to patch statistics, `get_stats` can return (1) the
temperature dataset in a dataframe format, and (2) a
`SpatialPolygonsDataFrame` of its hot and cold spots. The function
`plot_patches` can then recreate the original thermal image overlaid
with outlines of hot and cold spots, as well as the temperature
distribution if `plot_distribution = TRUE` (Figure 2).

    plot_patches(
        # The raw temperature data
        df = flir_stats$df,
        # The patch outlines
        patches = flir_stats$patches)

<img src="overview_files/figure-markdown_strict/fig-2-1.png" alt="Figure 2: The output of `plot_patches` includes a histogram and the original temperature data overlaid with outlines of hot and cold spots, identified using the G* variant of the Getis-Ord local statistic."  />
<p class="caption">
Figure 2: The output of `plot_patches` includes a histogram and the
original temperature data overlaid with outlines of hot and cold spots,
identified using the G\* variant of the Getis-Ord local statistic.
</p>

References
==========

Faye, E., Rebaudo, F., Yánez-Cajo, D., Cauvy-Fraunié, S., Dangles, O.,
2016. A toolbox for studying thermal heterogeneity across spatial
scales: From unmanned aerial vehicle imagery to landscape metrics.
Methods in Ecology and Evolution 7, 437–446.
doi:[10.1111/2041-210X.12488](https://doi.org/10.1111/2041-210X.12488)

Getis, A., Ord, J.K., 1996. Local spatial statistics: An overview.
Spatial analysis: modelling in a GIS environment 374, 261–277.

McGarigal, K., Cushman, S.A., Ene, E., 2012. FRAGSTATS v4: Spatial
pattern analysis program for categorical and continuous maps. Computer
software program produced by the authors at the University of
Massachusetts, Amherst. Available at:
[Http://www.umass.edu/landeco/research/fragstats/fragstats.html](http://www.umass.edu/landeco/research/fragstats/fragstats.html).

McGuire, J.L., Lawler, J.J., McRae, B.H., Theobald, D.M., 2016.
Achieving climate connectivity in a fragmented landscape. Proceedings of
the National Academy of Sciences 113, 7195–7200.
doi:[10.1073/pnas.1602817113](https://doi.org/10.1073/pnas.1602817113)

Shi, H., Wen, Z., Paull, D., Guo, M., 2016. A framework for quantifying
the thermal buffering effect of microhabitats. Biological Conservation
204, 175–180.
doi:[10.1016/j.biocon.2016.11.006](https://doi.org/10.1016/j.biocon.2016.11.006)

Tattersall, G.J., 2019. Thermimage: Thermal Image Analysis. Available
at:
[Https://cran.r-project.org/package=Thermimage](https://CRAN.R-project.org/package=Thermimage).
doi:[10.5281/zenodo.1069704](https://doi.org/10.5281/zenodo.1069704)

VanDerWal, J., Falconi, L., Januchowski, S., Shoo, L., Storlie, C.,
2014. SDMTools: Species distribution modelling tools: Tools for
processing data associated with species distribution modelling
exercises. Available at:
[Https://cran.r-project.org/package=SDMTools](https://CRAN.R-project.org/package=SDMTools).
