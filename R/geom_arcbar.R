#' Create an Arc-Barchart
#' 
#' An arc bar diagram that allows for spacing between the individual arc components 
#' and spans 180 degrees.
#' 
#' @section Aesthetics:
#' geom_arcbar understands the following aesthetics (required aesthetics are in bold):
#' 
#' - **shares**
#' - **r0** - inner radius
#' - **r1** - outer radius
#' - color
#' - fill
#' - linetype
#' - alpha 
#'   
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::stat_identity
#' @param n The number of
#' @param sep Separation between the different shares, as a total proportion of pi.
#' @export
#' 
#' @examples
#' bt <- data.frame(
#'   parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD", 
#'                      "Linke", "Gruene", "Fraktionslos"),
#'                    levels = c("CDU", "CSU", "AfD", "FDP", "SPD", 
#'                               "Linke", "Gruene", "Fraktionslos")),
#'   seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
#'   colors  = c("black", "blue", "lightblue", "yellow", "red",
#'               "purple", "green", "grey"),
#'   stringsAsFactors = FALSE)
#' 
#' ggplot(bt) + 
#'   geom_arc_bar(aes(shares = seats, r0 = 5, r1 = 10, fill = parties)) + 
#'   scale_fill_manual(values = bt$colors) +
#'   coord_fixed() +
#'   theme_void()
geom_arcbar <- function(mapping = NULL, data = NULL, stat = "arc_bar",
                         position = "identity", n = 360, sep = 0.05, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomArcbar,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, sep = sep, ...))
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomArcbar <- ggproto("GeomArcbar", ggplot2::GeomPolygon,
                      default_aes = list(colour = "black", fill = NA, size = 0.5, linetype = 1, alpha = NA)
)