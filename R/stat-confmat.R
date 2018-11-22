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
    dat <- table(data$x, data$y)
    if (isTRUE(normalize)) {
      dat <- dat / rowSums(dat)
    }
    dat <- as.data.frame(dat)
    colnames(dat)[seq(2)] <- c("x", "y")
    dat[, seq(2)] <- lapply(dat[, seq(2)], function(x) as.numeric(as.character(x)))
    dat$PANEL <- data$PANEL[1]
    dat$group <- seq_v(dat)
    dat
  }
)

#' @rdname geom_confmat
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