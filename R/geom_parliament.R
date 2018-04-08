#' Create a Parliament Diagram
#'
#' Draws a parliament diagram based on parties' member counts, 
#' where each point in the arc represents a single member of parliament. 
#' Parties are plotted right-to-left.
#' 
#' @section Aesthetics:
#' geom_parliament understands the following aesthetics (required aesthetics are in bold):
#' - **seats** - number of seats of the parties
#' - fill 
#' - color
#' 
#' @inheritParams ggplot2::geom_polygon
#' @param r0 Inner radius, defaults to 1.5.
#' @param r1 Outer radius, defaults to 3.
#' @param n Number of passed to `StatCircle`, defaults to 360.
#' 
#' @export
#' 
#' @examples 
#' bt <- data.frame(
#'   parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD", "Linke", "Gruene", "Fraktionslos"),
#'                    levels = c("CDU", "CSU", "AfD", "FDP", "SPD", "Linke", "Gruene", "Fraktionslos")),
#'   seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
#'   colors  = c("black", "blue", "lightblue", "yellow", "red","purple", "green", "grey"),
#'   stringsAsFactors = FALSE)
#' 
#' ggplot(bt) + geom_parliament(aes(seats = seats, fill = parties), color = "black") + 
#'   scale_fill_manual(values = bt$colors, labels = bt$parties) +
#'   coord_fixed() + 
#'   theme_void()
geom_parliament <- function(mapping = NULL, data = NULL, stat = "parliament",
                            position = "identity", r0 = 1.5, r1 = 3, n = 360, 
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomParliament,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, r0 = r0, r1 = r1, ...))
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomParliament <- ggproto("GeomParliament", ggplot2::GeomPolygon,
                          default_aes = list(colour = "black", fill = NA, 
                                             size = 0.5, linetype = 1, alpha = NA)
)