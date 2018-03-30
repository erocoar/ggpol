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