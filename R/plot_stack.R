#' plot_stack
#'
#' Create violin plots of raster stacks, such as those returned from 
#' \code{\link{stack_imgs}}. Useful to assess the distribution of data across
#' replicates e.g. over time.
#' @param img_stack The RasterStack to plot.
#' @param n The number of pixels to sample from each raster. Defaults to 100.
#' @param print_plot Should the resulting plots be printed? Defaults to FALSE.
#' @param save_plot Should the resulting plots be saved? Defaults to TRUE.
#' @param return_plot Should the resulting plots be returned? Defaults to FALSE.
#' @param out_dir Path to directory where plots should be saved (if applicable).
#' @param file_name Plot filename (if applicable).
#' @param file_ext File extension. Defaults to '.png'.
#' @param fig_width Figure width (if saved). Defaults to 8.
#' @param fig_height Figure height (if saved). Defaults to 9.
#' @param fig_units Figure dimension units (if saved). Defaults to "cm".
#' @param xlabel Option to specify the x axis label.
#' @param ylabel Option to specify the y axis label.
#' @param lab_size Size of axis labels. Defaults to 8.
#' @param text_size Size of axis text. Defaults to 6.
#' @param point_size Point size for raw data. Defaults to 0.5
#' @param quant_linetype Linetype for the upper and lower quantile lines. 
#' Defaults to "dotted".
#' @param quant_colour Line colour for the upper and lower quantile lines. 
#' Defaults to "black".
#' @param y_breaks Option to manually specify breaks in y axis. Defaults
#' to \code{waiver()}, where breaks are computed by the transformation object
#' (see \code{ggplot2::}\code{scale_colour_gradient}).
#' @param custom_theme Option to replace existing theme with a customised 
#' \code{ggplot} theme. See \code{ggplot2::}\code{theme}.
#' @importFrom rlang .data
#' @examples
#' plot_stack(img_stack = flir_stack, 
#'            n = 100,
#'            print_plot = TRUE,
#'            save_plot = FALSE,
#'            xlabel = "Time",
#'            ylabel = "Temperature",
#'            y_breaks = seq(25, 35, 1))
#'            
#' # Specify custom theme
#' custom_theme <- 
#'     theme_classic() + 
#'     theme(axis.title = element_text(size = 8),
#'           axis.text.x = element_text(size = 7,
#'           angle = 45,
#'           vjust = 0.5),
#'           axis.text.y = element_text(size = 7))
#'           
#' plot_stack(img_stack = flir_stack,
#'            print_plot = TRUE,
#'            save_plot = FALSE,
#'            custom_theme = custom_theme)
#' @export
#' @import ggplot2
#'
plot_stack <- function(img_stack,
                       n = 100,
                       print_plot = TRUE,
                       save_plot = FALSE,
                       return_plot = FALSE,
                       out_dir = NULL,
                       file_name = NULL,
                       file_ext = "png",
                       fig_width = 8,
                       fig_height = 9,
                       fig_units = "cm",
                       xlabel = NULL,
                       ylabel = NULL,
                       lab_size = 8,
                       text_size = 6,
                       point_size = 0.5,
                       quant_linetype = "dotted",
                       quant_colour = "black",
                       y_breaks = waiver(),
                       custom_theme = NULL) {
    
    group_sample <-
        raster::sampleRandom(img_stack, 
                             size = n, 
                             na.rm = TRUE)
    
    # Gather from wide to long
    group_sample <- reshape2::melt(group_sample)[,2:3]
    colnames(group_sample) <- c("rep_id", "val")
    
    # If rep_id begins with 'X', remove
    group_sample$rep_id <- gsub("^X", "", group_sample$rep_id)
    
    # Plot --------------------------------------------------------------------
    p <-
        ggplot(group_sample,
               aes(x = .data$rep_id, y = .data$val)) +
        geom_jitter(size = point_size,
                    shape = 21, 
                    fill = "grey",
                    colour = "transparent") +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
                    alpha = 0.5,
                    linetype = quant_linetype,
                    colour = quant_colour) +
        geom_violin(draw_quantiles = 0.5,
                    alpha = 0) +
        theme_classic() +
        theme(axis.title = element_text(size = lab_size),
              axis.text = element_text(size = text_size)) +
        scale_y_continuous(breaks = y_breaks)
    # Add labels
    if (!(is.null(xlabel))) {
        p <- p + xlab(xlabel)
    }
    if (!(is.null(ylabel))) {
        p <- p + ylab(ylabel)
    }
    # Customise theme
    if (!(is.null(custom_theme))) {
        p <- p + custom_theme
    }
    
    # Printing/saving plots ----------------------------------------------------
    
    if (print_plot) print(p)
    
    if (save_plot) {
        # Define file names
        if (is.null(file_name)) {
            file_name <-
                file.path(out_dir, paste("stack_plot", file_ext, sep = ""))
        }else{
            file_name <- file.path(out_dir, paste(file_name,
                                                  file_ext, 
                                                  sep = ""))
        }
        ggsave(plot = p, filename = file_name, dpi = 800, 
               width = fig_width, height = fig_height, units = fig_units)
    }
    
    # Return plot objects if required
    if (return_plot) return(p)
}
