#' A hybrid boxplot.
#'
#' Half boxplot, half scatterplot with customizable jitter.
#' @inheritParams ggplot2::geom_boxplot
#' 
#' @param outlier.intersect Defaults to `FALSE`. If set to `TRUE`, outliers will be part of the jitter-plot
#' (but keeping the given outlier graphical parameters) rather than plotted vertically above / below the whisker lines.
#' 
#' @param jitter.colour,jitter.color,jitter.fill,jitter.shape,jitter.size,jitter.stroke,jitter.alpha
#' Default aesthetics for jitter, set to `NULL` to inherit from the aesthetics used for the box.
#' 
#' @param jitter.width Width passed to position_jitter. Defaults to half the width of the boxplot.
#' 
#' @param jitter.height Height passed to position_jitter. Defaults to 40 percent of the resolution.
#' 
#' @param jitter.seed Seed passed to position_jitter for reproducible jittering.
#' 
#' @param boxplot.expand Defaults to `FALSE`. If set to `TRUE`, the full boxplots will be plotted.
#' 
#' @param errorbar.draw Draw horizontal whiskers at the top and bottom (the IQR). Defaults to `FALSE`.
#' 
#' @param errorbar.length Length of the horizontal whiskers (errorbar). Defaults to half the width of the half-boxplot,
#' or half the width of the entire boxplot if `boxplot.expand` is set to `TRUE`.
#' 
#' @importFrom ggplot2 layer position_dodge2
#' @export
#' @examples
#' set.seed(221)
#' df <- data.frame(score = rgamma(150, 4, 1), 
#'                  gender = sample(c("M", "F"), 150, replace = TRUE), 
#'                  genotype = factor(sample(1:3, 150, replace = TRUE)))
#' 
#' ggplot(df) + geom_boxjitter(aes(x = gender, y = score, fill = genotype),
#'                             jitter.shape = 21, jitter.color = NA, 
#'                             jitter.height = 0, jitter.width = 0.04,
#'                             outlier.color = NA, errorbar.draw = TRUE) +
#'   scale_fill_manual(values = c("#CF3721", "#31A9B8", "#258039")) +
#'   theme_minimal()
geom_boxjitter <- function(mapping = NULL, data = NULL,
                           stat = "BoxJitter", position = "dodge",
                           ...,
                           outlier.colour = NULL,
                           outlier.color = NULL,
                           outlier.fill = NULL,
                           outlier.shape = 19,
                           outlier.size = 1.5,
                           outlier.stroke = 0.5,
                           outlier.alpha = NULL,
                           outlier.intersect = FALSE,
                           jitter.colour = NULL,
                           jitter.color = NULL,
                           jitter.fill = NULL,
                           jitter.shape = 19,
                           jitter.size = 1.5,
                           jitter.stroke = 0.5,
                           jitter.alpha = NULL,
                           # jitter.width = NULL,
                           # jitter.height = NULL,
                           # jitter.seed = NULL,
                           jitter.position = ggplot2::PositionJitter, # TODO import,
                           jitter.params = list("width" = NULL, "height" = NULL),
                           boxplot.expand = FALSE,
                           notch = FALSE,
                           notchwidth = 0.5,
                           varwidth = FALSE,
                           errorbar.draw = FALSE,
                           errorbar.length = 0.5,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxJitter,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      outlier.intersect = outlier.intersect,
      jitter.colour = jitter.color %||% jitter.colour,
      jitter.fill = jitter.fill,
      jitter.shape = jitter.shape,
      jitter.size = jitter.size,
      jitter.stroke = jitter.stroke,
      jitter.alpha = jitter.alpha,
      # jitter.width = jitter.width,
      # jitter.height = jitter.height,
      # jitter.seed = jitter.seed,
      jitter.position = jitter.position,
      jitter.params = jitter.params,
      boxplot.expand = boxplot.expand,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      errorbar.draw = errorbar.draw,
      errorbar.length = errorbar.length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 alpha ggproto GeomBoxplot aes GeomSegment GeomPoint GeomCrossbar resolution PositionJitter
#' @importFrom grid grobTree
#' @export
GeomBoxJitter <- ggproto("GeomBoxJitter2", GeomBoxplot,
  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, shape = 19, linetype = "solid"),
  
  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax"),
  
  draw_group = function(data, panel_params, coord, fatten = 2,
                        outlier.colour = NULL, outlier.fill = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL,
                        outlier.intersect = FALSE,
                        jitter.colour = NULL, jitter.fill = NULL,
                        jitter.shape = 19, jitter.size = 1.5,
                        jitter.stroke = 0.5, jitter.alpha = NULL,
                        # jitter.width = NULL, jitter.height = NULL,
                        # jitter.seed = NULL, 
                        jitter.position = ggplot2::PositionJitter,
                        jitter.params = list("width" = NULL, "height" = NULL),
                        boxplot.expand = FALSE,
                        notch = FALSE, notchwidth = 0.5, varwidth = FALSE,
                        errorbar.draw = FALSE, errorbar.length = 0.5) {
    
    xrange <- data$xmax - data$xmin
    
    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )
    
    whiskers <- data.frame(
      x = data$x,
      xend = data$x,
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      common,
      stringsAsFactors = FALSE
    )
    
    if (errorbar.draw) {
      if (errorbar.length > 1 | errorbar.length < 0) {
        stop("Error bar length must be between 0 and 1.")
      }
      error_length_add <- ((data$xmin + xrange / 2) - data$xmin) 
      if (boxplot.expand) { #last term ^2 if want to do both sides
        error_length_add <- error_length_add * (1 - errorbar.length)^2 
      } else { 
        error_length_add <- error_length_add * (1 - errorbar.length)
      }
      
      error_whiskers <- data.frame(
        x = data$xmin + error_length_add,
        xend = if (boxplot.expand) data$xmax - error_length_add else 
          (data$xmin + xrange / 2),
        y = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        alpha = NA,
        common,
        stringsAsFactors = FALSE
      )
    
      error_grob <- GeomSegment$draw_panel(error_whiskers, panel_params, coord)
    } else {
      error_grob <- NULL
    }

    box <- data.frame(
      xmin = data$xmin,
      xmax = if (boxplot.expand) data$xmax else data$xmin + xrange / 2,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      common,
      stringsAsFactors = FALSE
    )
    
    if (!boxplot.expand) {
      # jitter.width <- jitter.width %||% 
      #   (data$xmax - (box$xmax + (data$xmax - box$xmax) / 1.75))
      # jitter.height <- jitter.height %||% (
      #   resolution(data$jitter_y[[1]], zero = FALSE) * 0.4)
      # jitter_df <- data.frame(
      #   width = jitter.width,
      #   height = jitter.height
      #   )
      # if (!is.null(jitter.seed)) jitter_df$seed = jitter.seed
      
      jitter_position_data <- data.frame(x = box$xmax + (data$xmax - box$xmax) / 2,
                                         y = data$jitter_y[[1]], PANEL = 1, group = -1)
      if (is(jitter.position, "PositionJitter")) {
        jitter.params$width <- jitter.params$width %||% 
          (data$xmax - (box$xmax + (data$xmax - box$xmax) / 1.75))
        jitter.params$height <- jitter.params$height %||% 
          (resolution(data$jitter_y[[1]], zero = FALSE) * 0.4)

        jitter_positions <- jitter.position$compute_layer(jitter_position_data,
                                                          jitter.params) # TODO compute_panel for PositionJitter.
      } else {
        jitter_positions <- jitter.position$compute_panel(jitter_position_data, 
                                                          jitter.params) 
        # Scale back to original range
        jitter_positions$x <- ((data$xmax - 0.025) - box$xmax - 0.025) * (jitter_positions$x - 
          min(jitter_positions$x)) / (max(jitter_positions$x) - min(jitter_positions$x)) + box$xmax + 0.025
        
      }

      # jitter_positions <- PositionJitter$compute_layer(
      #   data.frame(x = box$xmax + (data$xmax - box$xmax) / 2,
      #              y = data$jitter_y[[1]]),
      #   jitter_df
      # )
  
      if (outlier.intersect & length(data$outliers[[1]]) >= 1) {
        outlier_inds <- which(data$jitter_y[[1]] %in% data$outliers[[1]])
        jitter_outlier <- jitter_positions[outlier_inds, ]
        jitter_positions <- jitter_positions[-outlier_inds, ]
      }
        
      jitt <- data.frame(
        x = jitter_positions$x,
        y = jitter_positions$y,
        colour = jitter.colour %||% data$colour[1],
        fill = jitter.fill %||% data$fill[1],
        shape = jitter.shape %||% data$shape[1],
        size = jitter.size %||% data$size[1],
        stroke = jitter.stroke %||% data$stroke[1],
        fill = NA,
        alpha = jitter.alpha %||% data$alpha[1],
        stringsAsFactors = FALSE
      )
      
      
      jitter_grob <- GeomPoint$draw_panel(jitt, panel_params, coord)
    } else {
      jitter_grob <- NULL
    }
    
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) { # TODO make outlier plotting optional
      outliers <- data.frame(
        y = if (outlier.intersect & !boxplot.expand) jitter_outlier$y else 
          data$outliers[[1]],
        x = if (outlier.intersect & !boxplot.expand) jitter_outlier$x else 
          data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1],
        stringsAsFactors = FALSE
      )
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }
    tree <- grobTree(
      outliers_grob,
      error_grob,
      jitter_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord),
      GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
    )
    tree$name <- grid::grobName(tree, "geom_boxjitter")
    tree
    # ggname("geom_boxjitter", grobTree(
    #   outliers_grob,
    #   error_grob,
    #   jitter_grob,
    #   GeomSegment$draw_panel(whiskers, panel_params, coord),
    #   GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
    # ))
  }
)
