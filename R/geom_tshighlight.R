#' Timeseries highlight
#'
#' This is a version of `GeomRect` that defaults to spanning the entire y-axis.
#' 
#' @section Aesthetics:
#' geom_tshighlight understands the following aesthetics (required aesthetics are in bold):
#' - xmin
#' - xmax 
#' - fill
#' - color
#' 
#' @inheritParams ggplot2::geomRect

#' 
#' @export
#' 
#' @examples 
#' ggplot() + geom_point(aes(x=1, y=17)) + geom_tshighlight(aes(xmin = 2, xmax= 5), fill="red")

GeomTshighlight <- ggproto("GeomRect", GeomRect,
  default_aes = aes(colour = NA, fill = "grey35", 
                    size = 0.5, linetype = 1, alpha = NA),
                    
  required_aes = c("xmin", "xmax"),
  
  setup_data = function(data, params) {
    data$ymin <- -Inf
    data$ymax <- Inf
    data
  },
 
  draw_panel = function(self, data, panel_params, coord) {

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )
      
      polys <- plyr::alply(data, 1, function(row) {
        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- as.data.frame(row[aesthetics],
                             stringsAsFactors = FALSE)[rep(1,5), ]
        
        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })
      
      ggplot2:::ggname("bar", do.call("grobTree", polys))
    } else {
      coords <- coord$transform(data, panel_params)
      ggplot2:::ggname("geom_rect", rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = gpar(
          col = coords$colour,
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          lineend = "butt"
        )
      ))
    }
  }
)

geom_tshighlight <- function(mapping = NULL, data = NULL, 
                             stat = "identity", position = "identity",
                             ..., na.rm = FALSE, show.legend = NA, 
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTshighlight,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}