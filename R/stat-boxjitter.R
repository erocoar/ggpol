#' @rdname ggpol-extensions
#' @importFrom ggplot2 ggproto Stat StatBoxplot
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatBoxJitter <- ggproto("StatBoxJitter", StatBoxplot,
   required_aes = c("y"),
   non_missing_aes = "weight",
   
   compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
     df <- StatBoxplot$compute_group(data, scales, width, na.rm, coef)
     df$jitter_y <- list(data$y)
     df
   }
)
