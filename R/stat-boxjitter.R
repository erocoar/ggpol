#' @importFrom ggplot2 ggproto Stat StatBoxplot
StatBoxJitter <- ggproto("StatBoxJitter", Stat,
   required_aes = c("x", "y"),
   non_missing_aes = "weight",
   
   compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
     df <- StatBoxplot$compute_group(data, scales, width, na.rm, coef)
     df$jitter_y <- list(data$y)
     df
   }
)
