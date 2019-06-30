#' plot_patches
#'
#' Plot hot and cold patches returned from \code{\link{get_patches}}.
#' @param df A dataframe returned from \code{\link{get_patches}}.
#' @param patches A SpatialPolygonsDataFrame returned from \code{\link{get_patches}}.
#' @param bg_poly An optional background polygon. Can be a SpatialPolygonsDataFrame
#' or fortified dataframe (see \code{ggplot2::}\code{\link[ggplot2]{fortify}})
#' with the variables 'long', 'lat' and 'group'.
#' @param bg_colour Colour of the background polygon.
#' @param facet_cols Number of facet columns. Defaults to NULL.
#' @param facet_rows Number of facet rows. Defaults to NULL.
#' @param plot_distribution Should a histogram be plotted? Defaults to TRUE.
#' @param print_plot Should the resulting plots be printed? Defaults to FALSE.
#' @param save_plot Should the resulting plots be saved? Defaults to TRUE.
#' @param return_plot Should the resulting plots be returned? Defaults to FALSE.
#' @param out_dir Path to directory where plots should be saved (if applicable).
#' @param file_name Prefix for plot filenames (if applicable). If none
#' specified, uses generic names 'distribution' and 'patches'.
#' @param file_ext File extension. Defaults to '.png'.
#' @param fig_width Figure width (if saved). Defaults to 8.
#' @param fig_height Figure height (if saved). Defaults to 9.
#' @param fig_units Figure dimension units (if saved). Defaults to "cm".
#' @param lab_size Size of axis labels. Defaults to 8.
#' @param text_size Size of axis text and legend text. Defaults to 6.
#' @param outline_size Size of the outline. Defaults to 0.7.
#' @param val_pal Colour palette to use for raster. Defaults to palette
#' derived from a FLIR jpeg: \code{c("black", "#050155", "#120172", "#3b008e",
#' "#7200a9", "#8f00a0","#ba187f", "#d9365b", "#ed5930","#f76323", "#fa8600",
#' "#f6a704","#fad61e", "#fad61e")}.
#' @param hatching Option to add hatching to patch polygons. Defaults to FALSE
#' @param hatch_density Option to specify density of hatching (hot spot value 
#' followed by cold spot value). Defaults to: \code{c(1, 2)}.
#' @param hatch_angle Option to specify angle of hatching (hot spot value 
#' followed by cold spot value). Defaults to: \code{c(45, 135)}.
#' @param hatch_size Line thickness of hatching. Defaults to 0.5
#' @param fill_breaks Option to manually specify breaks in colourbar. Defaults
#' to \code{waiver()}, where breaks are computed by the transformation object
#' (see \code{ggplot2::}\code{scale_colour_gradient}).
#' @param patch_cols Colours for the patch borders (hot spot colour followed by
#' cold spot colour). Defaults to: \code{c("mistyrose", "cornflowerblue")}.
#' @param patch_labs Labels to use in patch outline legend. Defaults to 'Hot
#' spots' and 'Cold spots'.
#' @param val_lab Label to describe the variable of interest - corresponds to
#' the x axis of the histogram, and the fill legend of the raster plot.
#' @importFrom rlang .data
#' @examples
#' # FLIR temperature matrix ---------------------------------------------------
#' # Find hot and cold patches
#' flir_results <-
#'     get_patches(img = flir11835$flir_matrix,
#'                 id = flir11835$photo_no)
#'
#' # Look at the results for individual pixels
#' head(flir_results$df)
#'
#' # Look at the patch statistics for hot and cold patches
#' flir_results$pstats
#'
#' # Plot the patches
#' sp::plot(flir_results$patches)
#'
#' # Plot using ThermStats::plot_patches with hatching
#' plot_patches(df = flir_results$df,
#'              patches = flir_results$patches,
#'              hatching = TRUE,
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#'              
#' # Plot using ThermStats::plot_patches without hatching
#' plot_patches(df = flir_results$df,
#'              patches = flir_results$patches,
#'              hatching = FALSE,
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#'              
#'  \dontrun{
#'  # FLIR facets --------------------------------------------------------------
#'  # Load raw data
#'  raw_dat <- flir_raw$raw_dat
#'  camera_params <- flir_raw$camera_params
#'  metadata <- flir_metadata
#'
#'  # Batch convert
#'  img_list <- batch_convert(raw_dat, write_results = FALSE)
#'  
#'  # Get patches
#'  patch_stats <-
#'      stats_by_group(img_list = img_list,
#'                     metadata = metadata,
#'                     idvar = "photo_no",
#'                     style = "C",
#'                     grouping_var = "rep_id",
#'                     round_val = 0.5,
#'                     sum_stats = c("mean", "max", "min"))
#'  
#'  # Plot
#'  plot_patches(df = patch_stats$df,
#'               patches = patch_stats$patches,
#'               print_plot = TRUE,
#'               save_plot = FALSE)  
#'
#' # Worldclim2 temperature raster ---------------------------------------------
#' # Dataset 'sulawesi_temp' represents mean January temperature for the
#' # island of Sulawesi
#'
#' # Define projection and extent
#' img_proj <- raster::projection(sulawesi_temp)
#' img_extent <- raster::extent(sulawesi_temp)
#'
#' # Find hot and cold patches
#' worldclim_results <-
#'  get_patches(img = sulawesi_temp,
#'              id = "sulawesi",
#'              style = "C",
#'              img_proj = img_proj,
#'              img_extent = img_extent)
#'
#' # Look at the results for individual pixels
#' head(worldclim_results$df)
#'
#' # Look at the patch statistics for hot and cold patches
#' worldclim_results$pstats
#'
#' # Plot the patches
#' sp::plot(worldclim_results$patches)
#'
#' # Plot using ThermStats::plot_patches
#' plot_patches(df = worldclim_results$df,
#'              patches = worldclim_results$patches,
#'              bg_poly = sulawesi_bg,
#'              bg_colour = "grey",
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#' }
#'
#' @export
#' @import ggplot2
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'
plot_patches <- function(df,
                         patches,
                         bg_poly = NULL,
                         bg_colour = "grey",
                         facet_cols = NULL,
                         facet_rows = NULL,
                         plot_distribution = TRUE,
                         print_plot = TRUE,
                         save_plot = FALSE,
                         return_plot = FALSE,
                         out_dir = NULL,
                         file_name = NULL,
                         file_ext = "png",
                         fig_width = 8,
                         fig_height = 9,
                         fig_units = "cm",
                         lab_size = 8,
                         text_size = 6,
                         outline_size = 0.7,
                         val_pal = c("black", "#050155", "#120172",
                                     "#3b008e", "#7200a9", "#8f00a0",
                                     "#ba187f", "#d9365b", "#ed5930",
                                     "#f76323", "#fa8600", "#f6a704",
                                     "#fad61e", "#fad61e"),
                         hatching = FALSE,
                         hatch_density = c(1, 2), 
                         hatch_angle = c(45, 135),
                         hatch_size = 0.5,
                         fill_breaks = waiver(),
                         patch_cols = c("mistyrose", "cornflowerblue"),
                         patch_labs = c("Hot spots", "Cold spots"),
                         val_lab = NULL) {
    
    # Define function to capitalise first letter
    simpleCap <- function(x) {
        s <- strsplit(as.character(x), " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    
    if (any(names(df) == "id")) {
        
        img_id_orig <- unique(df$id)
        img_id <- vapply(img_id_orig, simpleCap, FUN.VALUE = character(1))
        
        df$img_id <-
            factor(df$id,
                   # Don't change levels
                   levels = img_id_orig,
                   # Do change labels
                   labels = img_id)
    }
    
    # Fix patch outline colours
    names(patch_cols) <- patch_labs
    
    if (is.null(val_lab)) {
        val_lab <- paste("Temperature (", "\U00B0", "C)",
                         sep = "")
    }
    
    facet <- ifelse(is.list(patches), TRUE, FALSE)
    
    # Histogram --------------------------------------------------------------
    
    if (plot_distribution) {
        
        message("Plotting distribution")
        
        df$G_bin <- factor(df$G_bin, levels = c(0, 1, -1),
                           labels = c("Background", "Hot spots", "Cold spots"))
        
        p1 <-
            ggplot() +
            geom_histogram(data = df,
                           aes(x = .data$val, y = .data$..density..),
                           colour = "black", fill = "white") +
            geom_density(data = df,
                         aes(x = .data$val), 
                         alpha = 0.2, fill = "grey") +
            xlab(val_lab) +
            ylab("Density") +
            theme_bw() +
            theme(axis.title = element_text(size = lab_size),
                  axis.text = element_text(size = text_size),
                  panel.grid = element_blank(),
                  plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))
        if (facet) {
            p1 <- p1 + 
                facet_wrap(~ img_id, 
                           nrow = facet_rows, ncol = facet_cols, 
                           scales = "free")
        }
        
    }else{
        p1 <- NULL
    }
    
    # Image ----------------------------------------------------------
    
    message("Plotting image with hot and cold spots")
    
    if (facet) {
        # If facetting, need to apply this over multiple patch dfs
        patches <-
            lapply(patches, function(patches_i){
                
                lyr_id <- names(patches_i)
                names(patches_i) <- "layer"
                
                # Add comment attribute to correctly plot holes
                for (x in 1:length(patches_i@polygons)) {
                    comment(patches_i@polygons[[x]]) =
                        rgeos::createPolygonsComment(patches_i@polygons[[x]])
                }
                
                # If hatching desired, create here
                if (hatching) {
                    # If background polygon supplied, hatch that and clip 
                    # (this avoids hatching problems with very small/irregular
                    # polygons)
                    if (!(is.null(bg_poly))) {
                        # With hatching parameters for hot spots
                        bg_hatch_hot <- 
                            HatchedPolygons::hatched.SpatialPolygons(x = bg_poly, 
                                                                     density = hatch_density[1], 
                                                                     angle = hatch_angle[1])
                        # With hatching parameters for cold spots
                        bg_hatch_cold <- 
                            HatchedPolygons::hatched.SpatialPolygons(x = bg_poly, 
                                                                     density = hatch_density[2], 
                                                                     angle = hatch_angle[2])
                        
                        proj4string(bg_hatch_hot) <- proj4string(bg_poly)
                        proj4string(bg_hatch_cold) <- proj4string(bg_poly)
                        
                        # Clip to patches
                        hot_hatch_i <- 
                            suppressWarnings(
                                raster::intersect(bg_hatch_hot, 
                                                  patches_i[patches_i$layer == 1,]))
                        cold_hatch_i <- 
                            suppressWarnings(
                                raster::intersect(bg_hatch_cold, 
                                                  patches_i[patches_i$layer == -1,]))
                        
                        # If no background polygon supplied, hatch the patches directly
                    }else {
                        hot_hatch_i <- 
                            tryCatch({
                                HatchedPolygons::hatched.SpatialPolygons(x = patches_i[patches_i$layer == 1,], 
                                                                         density = hatch_density[1], 
                                                                         angle = hatch_angle[1])
                            }, error = function(e) {
                                if (grepl("positive length", e)) {
                                    stop("Hatching density of hot spots too low, please increase and try again")
                                }
                            })
                        cold_hatch_i <- 
                            tryCatch({
                                HatchedPolygons::hatched.SpatialPolygons(x = patches_i[patches_i$layer == -1,], 
                                                                         density = hatch_density[2], 
                                                                         angle = hatch_angle[2])
                            }, error = function(e) {
                                if (grepl("positive length", e)) {
                                    stop("Hatching density of cold spots too low, please increase and try again")
                                }
                            })
                    }
                    
                    # Fortify
                    hot_hatch_i <- fortify(hot_hatch_i)
                    cold_hatch_i <- fortify(cold_hatch_i)
                    
                    # Add id
                    hot_hatch_i$id <- patch_labs[1]
                    cold_hatch_i$id <- patch_labs[2]
                    
                    # If either is empty, make NULL
                    if (is.null(nrow(hot_hatch_i))) hot_hatch_i <- NULL
                    if (is.null(nrow(cold_hatch_i))) cold_hatch_i <- NULL
                    
                    # Merge
                    hatches <- rbind(hot_hatch_i, cold_hatch_i)
                    rm(hot_hatch_i, cold_hatch_i)
                    
                    # Add image ID
                    hatches_i$img_id <- lyr_id
                    
                }else{
                    hatches_i <- NULL
                }
                
                # Fortify patch polygons to dataframe
                patches_i <- fortify(patches_i, region = "layer")
                
                # Add image ID
                patches_i$img_id <- lyr_id
                
                return(list(patches_i = patches_i, hatches_i = hatches_i))
            })
        
        patches <- do.call(Map, c(f = rbind, patches))
        hatches <- patches$hatches_i
        patches <- patches$patches_i
        
        # Relevel
        patches$img_id <-
            factor(patches$img_id,
                   levels = img_id_orig,
                   labels = img_id)
        if (hatching) {
            hatches$img_id <-
                factor(hatches$img_id,
                       levels = img_id_orig,
                       labels = img_id)
        }
    }else{
        
        # Add comment attribute to correctly plot holes
        for (x in 1:length(patches@polygons)) {
            comment(patches@polygons[[x]]) =
                rgeos::createPolygonsComment(patches@polygons[[x]])
        }
        
        lyr_id <- names(patches)
        names(patches) <- "layer"
        
        # If hatching desired, create here
        if (hatching) {
            
            # If background polygon supplied, hatch that and clip 
            # (this avoids hatching problems with very small/irregular
            # polygons)
            if (!(is.null(bg_poly))) {
                # With hatching parameters for hot spots
                bg_hatch_hot <- 
                    HatchedPolygons::hatched.SpatialPolygons(x = bg_poly, 
                                                             density = hatch_density[1], 
                                                             angle = hatch_angle[1])
                # With hatching parameters for cold spots
                bg_hatch_cold <- 
                    HatchedPolygons::hatched.SpatialPolygons(x = bg_poly, 
                                                             density = hatch_density[2], 
                                                             angle = hatch_angle[2])
                
                proj4string(bg_hatch_hot) <- proj4string(bg_poly)
                proj4string(bg_hatch_cold) <- proj4string(bg_poly)
                
                # Clip to patches
                hot_hatch <- 
                    suppressWarnings(
                        raster::intersect(bg_hatch_hot, 
                                          patches[patches$layer == 1,]))
                cold_hatch <- 
                    suppressWarnings(
                        raster::intersect(bg_hatch_cold, 
                                          patches[patches$layer == -1,]))
                
                # If no background polygon supplied, hatch the patches directly
            }else {
                
                hot_hatch <- 
                    tryCatch({
                        HatchedPolygons::hatched.SpatialPolygons(x = patches[patches$layer == 1,], 
                                                                 density = hatch_density[1], 
                                                                 angle = hatch_angle[1])
                    }, error = function(e) {
                        if (grepl("positive length", e)) {
                            stop("Hatching density of hot spots too low, please increase and try again")
                        }else {
                            stop(e)
                        }
                    })
                cold_hatch <- 
                    tryCatch({
                        HatchedPolygons::hatched.SpatialPolygons(x = patches[patches$layer == -1,], 
                                                                 density = hatch_density[2], 
                                                                 angle = hatch_angle[2])
                    }, error = function(e) {
                        if (grepl("positive length", e)) {
                            stop("Hatching density of cold spots too low, please increase and try again")
                        }else {
                            stop(e)
                        }
                    })
            }
            
            # Fortify
            hot_hatch <- fortify(hot_hatch)
            cold_hatch <- fortify(cold_hatch)
            
            # Add id
            hot_hatch$id <- patch_labs[1]
            cold_hatch$id <- patch_labs[2]
            
            # If either is empty, make NULL
            if (is.null(nrow(hot_hatch))) hot_hatch <- NULL
            if (is.null(nrow(cold_hatch))) cold_hatch <- NULL
            
            # Merge
            hatches <- rbind(hot_hatch, cold_hatch)
            rm(hot_hatch, cold_hatch)
        }
        
        # Fortify patch polygons to dataframe
        patches <- fortify(patches, region = "layer")
    }
    
    # Get rid of the background patch
    patches <- patches[patches$id != 0,]
    
    # Assign patch category to hot or cold spot
    patches$id <- factor(patches$id, levels = c(1, -1),
                         labels = patch_labs)
    
    # Create the plot
    p2 <- ggplot() +
        geom_raster(data = df,
                    aes(x = .data$x, 
                        y = .data$y, 
                        fill = .data$val)) +
        geom_polygon(data = patches,
                     aes(y = .data$lat, 
                         x = .data$long, 
                         group = .data$group, 
                         colour = .data$id),
                     alpha = 0, size = outline_size) +
        theme_bw() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.box.spacing = unit(0,"cm"),
              legend.position = "bottom",
              legend.title = element_text(size = text_size),
              legend.text = element_text(size = text_size),
              legend.key = element_rect(fill = "black"),
              panel.grid = element_blank(),
              plot.margin = margin(0.1, 0.1, 0, 0, unit = "cm")) +
        scale_fill_gradientn(colours = val_pal,
                             breaks = fill_breaks) +
        scale_colour_manual(values = patch_cols, name = NULL) +
        guides(fill = guide_colorbar(order = 1,
                                     title = val_lab,
                                     title.position = "top",
                                     title.hjust = 0.5,
                                     barheight = 0.8),
               colour = guide_legend(order = 2,
                                     keywidth = 0.7,
                                     keyheight = 0.7,
                                     direction = "vertical",
                                     override.aes = list(alpha = 1,
                                                         fill = "black",
                                                         size = 0.8))) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0,0))
    
    # If background polygon is supplied:
    if (!(is.null(bg_poly))) {
        p2 <- p2 +
            geom_polygon(data = bg_poly,
                         aes(x = .data$long,
                             y = .data$lat,
                             group = .data$group),
                         colour = bg_colour,
                         alpha = 0)
    }
    
    # If hatching is required:
    if (hatching) {
        p2 <- p2 +
            geom_line(data = hatches,
                      aes(y = .data$lat,
                          x = .data$long, 
                          group = .data$group, 
                          colour = .data$id),
                      size = hatch_size)
    }
    
    # If facetting is required:
    if (facet) {
        p2 <- p2 +
            facet_wrap(~ img_id, nrow = facet_rows, ncol = facet_cols) +
            theme()
        
        # If plotting facetted distribution as well, move the colourbar
        if(plot_distribution){
            val_lab <- paste("Temperature\n(", "\U00B0", "C)",
                             sep = "")
            p2 <-
                p2 +
                theme(axis.title = element_text(colour = "transparent",
                                                size = lab_size),
                      axis.text = element_text(colour = "transparent",
                                               size = text_size),
                      axis.ticks = element_line(colour = "transparent"),
                      legend.box.spacing = unit(0.1,"cm"),
                      legend.position = "right",
                      plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm")) +
                guides(fill = guide_colorbar(order = 1,
                                             title = val_lab,
                                             title.position = "top",
                                             title.hjust = 0.5,
                                             direction  = "vertical",
                                             barwidth = 0.8),
                       colour = guide_legend(order = 2,
                                             keywidth = 0.7,
                                             keyheight = 0.7,
                                             direction = "vertical",
                                             override.aes = list(alpha = 1,
                                                                 fill = "black",
                                                                 size = 0.8)))
        }
    }
    
    # Printing/saving plots -----------------------------------------------------
    
    if (print_plot) {
        
        if (requireNamespace("gridExtra", quietly = TRUE)) {
            
            if (plot_distribution) {
                suppressMessages(
                    gridExtra::grid.arrange(p1, p2, ncol = 2)
                )
            } else{
                print(p2)
            }
            
        } else {
            suppressMessages(print(p1))
            print(p2)
        }
    }
    
    if (save_plot) {
        
        # Define file names
        if (is.null(file_name)) {
            p1_filename <-
                file.path(out_dir, paste("distribution.",file_ext, sep = ""))
            p2_filename <-
                file.path(out_dir, paste("patches.",file_ext, sep = ""))
        }else{
            
            p1_filename <-
                file.path(out_dir, paste(file_name,
                                         "_distribution.",file_ext, sep = ""))
            p2_filename <-
                file.path(out_dir, paste(file_name, "_patches.",file_ext, sep = ""))
            
        }
        
        if (plot_distribution) {
            # Plot distribution
            suppressMessages(
                ggsave(plot = p1, filename = p1_filename,
                       dpi = 800, width = fig_width, height = fig_height, units = fig_units)
            )
        }
        
        # Plot image w patches
        ggsave(plot = p2, filename = p2_filename,
               dpi = 800, width = fig_width, height = fig_height, units = fig_units)
        
    }
    
    # Return plot objects if required
    if (return_plot) {
        if (plot_distribution) {
            return(list(fig_distribution = p1, fig_patches = p2))
        } else{
            return(fig_patches = p2)
        }
        
    }
    
    
}
