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
#' @inheritParams ggplot2::geomPolygon
#' @param r0 Inner radius
#' @param r1 Outer radius
#' @param n Number of passed to `StatCircle`
#' 
#' @export
#' 
#' @examples 
#' # Generate Data
#' bt <- data.frame(
#'         parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
#'         seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#'         colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
#'         stringsAsFactors = FALSE)
#' ggplot(bt) + geom_parliament(aes(seats = seats, fill = parties, color = parties)) + coord_fixed() + theme_void()

GeomParliament <- ggproto("GeomParliament", GeomPolygon,
                          default_aes = list(colour = "black", fill = NA, 
                                             size = 0.5, linetype = 1, alpha = NA)
)

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