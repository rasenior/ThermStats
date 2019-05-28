# ThermStats

Quantify thermal heterogeneity using gridded temperature data in R. 

Variation in temperature at fine spatiotemporal scales strongly influences physiological, behavioural and demographic responses to environmental change. This `R` package addresses current constraints on applying thermography in ecology, by speeding up and simplifying the extraction of data from (FLIR) thermal images, and by facilitating the calculation of different metrics of thermal heterogeneity for any gridded temperature data. 

Data extraction and processing of FLIR thermal images relies heavily on [`Thermimage`](https://CRAN.R-project.org/package=Thermimage "Thermimage package on CRAN"). The `ThermStats` function `batch_extract` uses two functions in particular (`Thermimage::readflirJPG` and `Thermimage::flirsettings`) that also require the external software ExifTool. Installation instructions for ExifTool can be found here: http://www.sno.phy.queensu.ca/~phil/exiftool/install.html
   

Analytical functions and metrics take inspiration from: [FRAGSTATS](http://www.umass.edu/landeco/research/fragstats/documents/fragstats.help.4.2.pdf), [`SDMTools`](https://CRAN.R-project.org/package=SDMTools),[Faye et al. 2016](https://doi.org/10.1111/2041-210X.12488), [Shi et al. 2016](https://doi.org/10.1016/j.biocon.2016.11.006), [McGuire et al. 2016](https://doi.org/10.1073/pnas.1602817113) and [Senior et al. 2018](https://doi.org/10.1111/gcb.13914).

See the package vignette for details and worked examples.

## References

Faye, E., Rebaudo, F., Yánez‐Cajo, D., Cauvy‐Fraunié, S., Dangles, O. and Tatem, A. (2016), A toolbox for studying thermal heterogeneity across spatial scales: from unmanned aerial vehicle imagery to landscape metrics. Methods Ecol Evol, 7: 437-446. https://doi.org/10.1111/2041-210X.12488

McGarigal, K., Cushman, S.A., Neel, M.C., and Ene, E. (2002). FRAGSTATS: Spatial Pattern Analysis Program for Categorical Maps. Computer software program produced by the authors at the University of Massachusetts, Amherst. www.umass.edu/landeco/research/fragstats/fragstats.html

McGuire, J.L., Lawler, J.J., McRae, B.H., Nuñez, T.A., and Theobald, D.M. (2016). Achieving climate connectivity in a fragmented landscape. PNAS, 113: 7195-7200. https://doi.org/10.1073/pnas.1602817113

Senior, R.A., Hill, J.K., Benedick, S. and Edwards, D.P. (2018). Tropical forests are thermally buffered despite intensive selective logging. Glob Change Biol. 24:1267–1278. https://doi.org/10.1111/gcb.13914

Shi, H., Wen, Z., Paull, D., Guo, M. (2016). A framework for quantifying the thermal buffering effect of microhabitats. Biological Conservation, 204: 175-180. https://doi.org/10.1016/j.biocon.2016.11.006

Tattersall, G.J. (2018). Thermimage: Thermal Image Analysis. doi: 10.5281/zenodo.1069704. R package version 3.1.2. https://CRAN.R-project.org/package=Thermimage

VanDerWal, J., Falconi, L., Januchowski, S., Shoo, L., and Storlie, C. (2014). SDMTools: Species Distribution Modelling Tools: Tools for processing data associated with species distribution modelling exercises. R package version 1.1-221. https://CRAN.R-project.org/package=SDMTools
