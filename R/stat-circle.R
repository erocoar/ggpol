StatCircle <- ggproto("StatCircle", Stat, 
  required_aes = c("x", "y", "r"),
  
  compute_panel = function(data, scales, n = 360) {
    angle <- seq(-pi, pi, length.out = n)
    df <- do.call(rbind,
      lapply(1:nrow(data), function(i) {
        data.frame(x = data$x[i] + data$r[i] * cos(angle),
                   y = data$y[i] + data$r[i] * sin(angle),
                   group = i)
        })
      )
    
    add_cols <- setdiff(c(colnames(data), "PANEL", "r"), colnames(df))
    for (col_ in add_cols) {
      df[, col_] <- rep(data[, col_], rle(df$group)$lengths)
    }
    df
    }
)

stat_circle <- function(mapping = NULL, data = NULL, geom = "circle",
                        position = "identity", n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatCircle,
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(n = n, na.rm = na.rm, ...)
  )
}