#' Points with jitter for half geoms.
#'
#' @inheritParams ggplot2::geom_boxplot
#' @importFrom ggplot2 layer position_dodge2 new_data_frame aes
#' @export
#' @examples

geom_half_point <- function(
  mapping = NULL, data = NULL,
  stat = "HalfPoint", position = "identity",
  ...,
  side = "r",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomHalfPoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        side = side,
        na.rm = na.rm,
        ...
      )
    )
}

GeomHalfPoint <- ggproto(
  "GeomHalfPoint", 
  Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, 
    colour = "black", 
    size = 1.5, 
    fill = NA,
    alpha = NA, 
    stroke = 0.5
    ),
  
  setup_data = function(data, params) {
    x_data    <- GeomBoxplot$setup_data(data, NULL)
    data$xmin <- x_data$xmin
    data$xmax <- x_data$xmax
    data
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE, side = "r") {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
      }
    xrange <- data$xmax - data$xmin
    x_add  <- data$x + (xrange / 2) * switch((side == "r") + 1, -1, 1)
    data$x <- data$x + x_add
    dd1 <<- data
    pp1 <<- panel_params
    coords <- coord$transform(data, panel_params)
    cc <<- coords
    
    pointgrob <- pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,
      gp = gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill, coords$alpha),
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )
    pointgrob$name <- "geom_half_point"
    pointgrob
  }
)
