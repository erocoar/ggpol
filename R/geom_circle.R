#' Circles with pre-defined radii.
#' 
#' Similar to `[geom_point()]`, but allows for full control of the size.
#' 
#' @section Aesthetics:
#' geom_circle understands the following aesthetics (required aesthetics are in bold):
#' 
#' - **x** - x-coordinate of center
#' - **y** - y-coordinate of center
#' - **r** - radius
#' - color
#' - fill
#' - linetype
#' - alpha 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param n The number of points calculated for the circle polygon, defaults to 360.
#' 
#' @importFrom ggplot2 layer
#' 
#' @export
#' 
#' @examples 
#' set.seed(22189)
#' df <- data.frame(x = sample(1:10, 3), y = sample(1:10, 3), 
#'                  r = sample(3:4, 3, replace = TRUE))
#' 
#' ggplot(df) + geom_circle(aes(x = x, y = y, r = r, fill = gl(3, 1))) +
#'   coord_fixed()
geom_circle <- function(mapping = NULL, data = NULL, stat = "circle", 
                        position = "identity", n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomCircle,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPolygon
#' @export
GeomCircle <- ggproto("GeomCircle", GeomPolygon,
  default_aes = list(colour = "black", fill = NA, size = 0.5, linetype = 1, alpha = NA)
)
