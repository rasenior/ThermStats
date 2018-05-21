#' plot_patches
#'
#' Plot hot and cold spots.
#' @param df A dataframe returned from \code{get_patches}.
#' @param patches A SpatialPolygonsDataFrame returned from \code{get_patches}.
#' @param plot_distribution Should a histogram be plotted? Defaults to TRUE.
#' @param print_plot Should the resulting plots be printed? Defaults to FALSE.
#' @param save_plot Should the resulting plots be saved? Defaults to TRUE.
#' @param return_plot Should the resulting plots be returned? Defaults to FALSE.
#' @param out_dir Path to directory where plots should be saved (if applicable).
#' @param file_name Prefix for plot filenames (if applicable). If none
#' specified, uses generic names 'distribution' and 'patches'.
#' @param file_ext File extension. Defaults to '.png'.
#' @param lab_size Size of axes labels. Defaults to 8.
#' @param text_size Size of axes text and legend text. Defaults to 6.
#' @param fig_width Figure width (if saved). Defaults to 8.
#' @param fig_height Figure height (if saved). Defaults to 9.
#' @param fig_units Figure dimension units (if saved). Defaults to "cm".
#' @param val_pal Colour palette to use for raster. Defaults to palette
#' derived from a FLIR jpeg: \code{c("black", "#050155", "#120172", "#3b008e",
#' "#7200a9", "#8f00a0","#ba187f", "#d9365b", "#ed5930","#f76323", "#fa8600",
#' "#f6a704","#fad61e", "#fad61e")}.
#' @param patch_cols Colours for the patch borders (hot spot colour followed by
#' cold spot colour). Defaults to: \code{c("mistyrose", "cornflowerblue")}.
#' @param patch_labs Labels to use in patch outline legend. Defaults to 'Hot
#' spots' and 'Cold spots'.
#' @param val_lab Label to describe the variable of interest - corresponds to
#' the x axis of the histogram, and the fill legend of the raster plot.
#' @examples
#' # Find hot and cold spots
#' results <- get_patches(mat = flir11835$flir_matrix,
#'                        matrix_id = flir11835$matrix_id)
#'
#' # Plot
#' df <- results$df
#' patches <- results$patches
#' plot_patches(df = df,
#'              patches = patches,
#'              print_plot = TRUE,
#'              save_plot = FALSE)
#'
#' @export
#' @import ggplot2
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'
plot_patches <- function(df,
                         patches,
                         plot_distribution = TRUE,
                         print_plot = FALSE,
                         save_plot = TRUE,
                         return_plot = FALSE,
                         out_dir = NULL,
                         file_name = NULL,
                         file_ext = "png",
                         lab_size = 8,
                         text_size = 6,
                         fig_width = 8,
                         fig_height = 9,
                         fig_units = "cm",
                         val_pal = c("black", "#050155", "#120172",
                                      "#3b008e", "#7200a9", "#8f00a0",
                                      "#ba187f", "#d9365b", "#ed5930",
                                      "#f76323", "#fa8600", "#f6a704",
                                      "#fad61e", "#fad61e"),
                         patch_cols = c("mistyrose", "cornflowerblue"),
                         patch_labs = c("Hot spots", "Cold spots"),
                         val_lab = NULL) {

  matrix_id <- unique(df$matrix_id)
  names(patch_cols) <- patch_labs

  if(is.null(val_lab)){
    val_lab <- paste("Temperature (", "\U00B0", "C)",
                     sep = "")
  }

  # Histogram --------------------------------------------------------------

  if(plot_distribution){

  message("Plotting distribution")

  df$G_bin <- factor(df$G_bin, levels = c(0, 1, -1),
                          labels = c("Background", "Hot spots", "Cold spots"))

  p1 <-
    ggplot() +
    geom_histogram(data = df,
                   aes(x = val, y = ..density..),
                   colour = "black", fill = "white") +
    geom_density(data = df,
                 aes(x = val), alpha = 0.2, fill = "grey") +
    xlab(val_lab) +
    ylab("Density") +
    theme_bw() +
    theme(axis.title = element_text(size = lab_size),
          axis.text = element_text(size = text_size),
          panel.grid = element_blank(),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"))

  }else{
    p1 <- NULL
  }

  # Image ----------------------------------------------------------

  message("Plotting image with hot and cold spots")

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

  # Create the plot
  p2 <- ggplot() +
    geom_raster(data = df,
                aes(x = x, y = y, fill = val)) +
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
    scale_fill_gradientn(colours = val_pal) +
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

    if(plot_distribution){
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
  if(return_plot){
    if(plot_distribution){
      return(list(fig_distribution = p1, fig_patches = p2))
    } else{
      return(fig_patches = p2)
    }

  }


}
