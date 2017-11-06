#' plot_patches
#'
#' Plot hot and cold spots from a FLIR thermal image.
#' @param flir_df A dataframe returned from \code{get_patches}.
#' @param patches A SpatialPolygonsDataFrame returned from \code{get_patches}.
#' @param print_plot Should the resulting plots be printed? Defaults to FALSE.
#' @param save_plot Should the resulting plots be saved? Defaults to TRUE.
#' @param out_dir Path to directory where plots will be saved.
#' @param lab_size Size of axes labels. Defaults to 8.
#' @param text_size Size of axes text and legend text. Defaults to 6.
#' @param temp_pal Colour palette to use for temperature. Defaults to palette derived from a FLIR jpeg.
#' @param patch_cols Colours for the patch borders (hot spot colour followed by cold spot colour).
#' @examples
#' # Find hot and cold spots
#' results <- get_patches(flir_matrix = flir11835$flir_matrix,photo_no = flir11835$photo_no)
#'
#' # Plot
#' flir_df <- results$flir_df
#' patches <- results$patches
#' plot_patches(flir_df = flir_df, patches = patches, print_plot = TRUE, save_plot = FALSE)
#'
#' @export
#' @import ggplot2
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'
plot_patches <- function(flir_df,
                         patches,
                         plot_distribution = TRUE,
                         print_plot = FALSE,
                         save_plot = TRUE,
                         return_plot = FALSE,
                         out_dir,
                         file_name = NULL,
                         file_ext = "png",
                         lab_size = 8,
                         text_size = 6,
                         fig_width = 8,
                         fig_height = 9,
                         fig_units = "cm",
                         temp_pal = c("black", "#050155", "#120172",
                                      "#3b008e", "#7200a9", "#8f00a0",
                                      "#ba187f", "#d9365b", "#ed5930",
                                      "#f76323", "#fa8600", "#f6a704",
                                      "#fad61e", "#fad61e"),
                         patch_cols = c("mistyrose", "cornflowerblue"),
                         patch_labs = c("Hot spots", "Cold spots")) {

  photo_no <- unique(flir_df$photo_no)

  # Histogram --------------------------------------------------------------

  if(plot_distribution){

  message("Plotting temperature distribution")

  flir_df$G_bin <- factor(flir_df$G_bin, levels = c(0, 1, -1),
                          labels = c("Background", "Hot spots", "Cold spots"))

  p1 <-
    ggplot() +
    geom_histogram(data = flir_df,
                   aes(x = temp, y = ..density..),
                   colour = "black", fill = "white") +
    geom_density(data = flir_df,
                 aes(x = temp), alpha = 0.2, fill = "grey") +
    xlab(expression(paste("Temperature (", degree * C, ")", sep = ""))) +
    ylab("Density") +
    theme_bw() +
    theme(axis.title = element_text(size = lab_size),
          axis.text = element_text(size = text_size),
          panel.grid = element_blank(),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))

  }else{
    p1 <- NULL
  }

  # Thermal image ----------------------------------------------------------

  message("Plotting thermal image with hot and cold spots")

  # Add comment attribute to correctly plot holes
  for (x in 1:length(patches@polygons)) {
    comment(patches@polygons[[x]]) =
      rgeos::createPolygonsComment(patches@polygons[[x]])
  }

  # Fortify patch polygons to dataframe
  patches <- fortify(patches, region = "layer")

  # Get rid of the background patch
  patches <- patches[patches$id != 0, ]

  # Assign patch category to hot or cold spot
  patches$id <- factor(patches$id, levels = c(1, -1),
                       labels = patch_labs)

  # Match coordinates up to those in the temperature dataframe
  patches$long <- patches$long * 160 + 0.5
  patches$lat <- patches$lat * 120 + 0.5

  # Mirror latitude (flips wrong way when fortified)
  patches$lat <- 121 - patches$lat

  # Create the plot
  p2 <- ggplot() +
    geom_raster(data = flir_df,
                aes(x = x, y = y, fill = temp)) +
    geom_polygon(data = patches,
                 aes(y = lat,x = long, group = group, colour = id),
                 alpha = 0, size = 0.7) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.box.spacing = unit(0,"cm"),
          legend.position = "bottom",
          legend.title = element_text(size = text_size),
          legend.text = element_text(size = text_size),
          legend.key = element_rect(fill = "black"),
          plot.margin = margin(0.1, 0.1, 0, 0, unit = "cm")) +
    scale_fill_gradientn(name = expression(paste("Temperature (",degree * C, ")",
                                                 sep = "")), colours = temp_pal) +
    scale_colour_manual(values = patch_cols, name = NULL) +
    guides(fill = guide_colorbar(order = 1,
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

  # Printing/saving plots -----------------------------------------------------

  if(isTRUE (print_plot)){

    if (requireNamespace("gridExtra", quietly = TRUE)) {

      if(plot_distribution){
        suppressMessages(
        gridExtra::grid.arrange(p1, p2, ncol=2)
        )
      } else{
        print(p2)
      }

    } else {
      suppressMessages(print(p1))
      print(p2)
    }
  }

  if(isTRUE (save_plot)){

    # Define file names
    if(is.null(file_name)){
      p1_filename <-
        file.path(out_dir, paste("FLIR", photo_no,
                                 "_distribution.",file_ext, sep = ""))
      p2_filename <-
        file.path(out_dir, paste("FLIR", photo_no, "_patches.",file_ext, sep = ""))
    }else{

      p1_filename <-
        file.path(out_dir, paste(file_name,
                                 "_distribution.",file_ext, sep = ""))
      p2_filename <-
        file.path(out_dir, paste(file_name, "_patches.",file_ext, sep = ""))

    }

    if(plot_distribution){
      # Plot distribution
      suppressMessages(
        ggsave(plot = p1, filename = p1_filename,
               dpi = 800, width = fig_width, height = fig_height, units = fig_units)
      )
    }

    # Plot thermal image w patches
    ggsave(plot = p2, filename = p2_filename,
           dpi = 800, width = fig_width, height = fig_height, units = fig_units)

  }

  # Return plot objects if required
  if(return_plot){
    if(plot_distribution){
      return(list(fig_distribution = p1, fig_thermal = p2))
    } else{
      return(fig_thermal = p2)
    }

  }


}
