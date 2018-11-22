#' Timeseries highlighting
#'
#' This is a version of [ggplot2::geom_rect()] that defaults to spanning the entirety of the y-axis.
#' 
#' @section Aesthetics:
#' geom_tshighlight understands the following aesthetics (required aesthetics are in bold):
#' - **xmin**
#' - **xmax**
#' - fill
#' - color
#' 
#' @inheritParams ggplot2::geom_rect
#' 
#' @importFrom ggplot2 layer
#'
#' @export
#' 
#' @examples 
#' ggplot(economics, aes(x = date, y = unemploy)) + 
#'   geom_line() +
#'   geom_tshighlight(aes(xmin = as.Date("01/01/1990", format = "%d/%m/%Y"), 
#'                        xmax = as.Date("01/01/2000", format = "%d/%m/%Y")),
#'                    alpha = 0.01)
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

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomRect GeomPolygon
#' @importFrom grid rectGrob gpar
#' @export
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
        poly <- ggplot2:::rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- as.data.frame(row[aesthetics],
                             stringsAsFactors = FALSE)[rep(1,5), ]
        
        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })
      
      # ggname("bar", do.call("grobTree", polys))
      tree <- do.call("grobTree", polys)
      tree$name <- grid::grobName(tree, "bar")
      tree
    } else {
      coords <- coord$transform(data, panel_params)
      tree <- rectGrob(
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
      )
      tree$name <- grid::grobName(tree, "geom_rect")
      tree
      
      # ggname("geom_rect", rectGrob(
      #   coords$xmin, coords$ymax,
      #   width = coords$xmax - coords$xmin,
      #   height = coords$ymax - coords$ymin,
      #   default.units = "native",
      #   just = c("left", "top"),
      #   gp = gpar(
      #     col = coords$colour,
      #     fill = alpha(coords$fill, coords$alpha),
      #     lwd = coords$size * .pt,
      #     lty = coords$linetype,
      #     lineend = "butt"
      #   )
      # ))
    }
  }
)