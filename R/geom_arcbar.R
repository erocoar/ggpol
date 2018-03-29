#' Create an Arc-Barchart
#' 
#' An arc bar diagram that allows for spacing between the individual arc components 
#' and spans 180 degrees.
#' 
#' @section Aesthetics:
#' geom_arc_bar understands the following aesthetics (required aesthetics are in bold):
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
#' # Generate Data
#' # Generate Data
#' bt <- data.frame(
#'         parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
#'         seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#'         colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
#'         stringsAsFactors = FALSE)
#' ggplot_parliament2(bt$parties, bt$seats, bt$colors, sep = 0.05)

GeomArcBar <- ggproto("GeomArcBar", GeomPolygon,
                      default_aes = list(colour = "black", fill = NA, size = 0.5, linetype = 1, alpha = NA)
)


geom_arc_bar <- function(mapping = NULL, data = NULL, stat = "arc_bar",
                         position = "identity", n = 360, sep = 0.05, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomArcBar,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, sep = sep, ...))
}


ggplot(bt) + geom_arc_bar(aes(shares = seats, r0 = 5, r1 = 10, fill = parties)) + coord_fixed()