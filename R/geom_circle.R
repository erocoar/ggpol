GeomCircle <- ggproto("GeomCircle", GeomPolygon,
  default_aes = list(colour = "black", fill = NA, size = 0.5, linetype = 1, alpha = NA)
)

geom_circle <- function(mapping = NULL, data = NULL, stat = "circle", 
                        position = "identity", n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomCircle,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}