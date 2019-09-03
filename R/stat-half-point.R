StatHalfPoint <- ggproto(
  "StatHalfPoint", StatBoxplot,
  required_aes = c("y"),
  non_missing_aes = "weight",
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
    df <- StatBoxplot$compute_group(data, scales, width, na.rm, coef)
    df$point_y <- list(data$y)
    df$point_x <- list(data$x)
    df$y <- df$x
    ddstat <<- df
    df
    }
)
