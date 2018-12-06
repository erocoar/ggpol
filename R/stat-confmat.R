#' @rdname ggpol-extensions
#' @importFrom ggplot2 ggproto Stat stat 
#' @format NULL
#' @usage NULL
#' @export
StatConfmat <- ggproto("StatConfmat", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(fill = stat(Freq)),

  compute_panel = function(
    data,
    scales,
    na.rm = FALSE,
    normalize = FALSE
  ) {
    dat <- as.data.frame(table(data$x, data$y))
    colnames(dat)[seq(2)] <- c("x", "y")
    dat$Normalized <- dat$Freq / tapply(dat$Freq, dat$x, sum)[dat$x]
    if (isTRUE(normalize)) {
      dat$Freq <- dat$Normalized
    }
    dat[, seq(3)] <- lapply(dat[, seq(3)], function(x) as.numeric(as.character(x)))
    dat$PANEL <- data$PANEL[1]
    dat$group <- seq_v(dat)
    dat
  }
)

#' @rdname GeomConfmat
#' @param normalize Boolean indicator for whether to scale the frequency values.
#' @inheritParams ggplot2::stat_identity
#' @importFrom ggplot2 layer
#' @export
stat_confmat  <- function(mapping = NULL, data = NULL, geom = "tile",
                          position = "identity", show.legend = NA, inherit.aes = TRUE,
                          na.rm = FALSE, normalize = FALSE, ...) {
  
  layer(
    stat = StatConfmat,
    mapping = mapping,
    data = data, 
    geom = geom,
    position = position, 
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(na.rm = na.rm, normalize = normalize, ...)
  )
}