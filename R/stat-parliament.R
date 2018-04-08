#' @param r0 inner radius of the arc. Defaults to 1.5
#' @param r1 outer radius of the arc. Defaults to 3
#' @param n number of points used to calculate individual circle polygons. Defaults to 360.
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#'   \item{x}{x coordinates of individual MPs}
#'   \item{y}{y coordinates of individual MPs}
#' }
#' 
#' @importFrom ggplot2 layer
#' 
#' @export
#' @rdname geom_parliament
stat_parliament  <- function(mapping = NULL, data = NULL, geom = "parliament",
                             position = "identity", r0 = 1.5, r1 = 3, n = 360, 
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatParliament,
    mapping = mapping,
    data = data, 
    geom = geom,
    position = position, 
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(na.rm = na.rm, n = n, r0 = r0, r1 = r1, ...)
  )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatParliament <- ggproto("StatParliament", Stat,
  required_aes = c("seats"),
  
  compute_panel = function(data, scales, r0, r1, n) {
    row_coords <- function(r, n) {
      angles <- seq(0, 0.5, length.out = n) * pi * 2
      matrix(c(cos(angles) * r, sin(angles) * r), ncol = 2)
    }
    
    total <- sum(data$seats)
    rows <- treated <- 0
    
    while (treated < total) {
      rows <- rows + 1
      point_rad <- 0.6 / rows
      arc_start <- arc_end <- point_rad
      row_centers <- seq(r0 + point_rad, 
                         r1 - point_rad, 
                         length.out = rows)
      row_sums <- floor(pi / (2 * asin(point_rad / (row_centers - point_rad))))
      treated <- sum(row_sums)
    }
    
    ratio <- if (treated > total) round((row_sums / sum(row_sums)) * total) else row_sums
    
    if (sum(ratio) != total) {
      diff <- sum(ratio) - total
      rs <- row_sums / sum(row_sums) * total
      idx <- order(rs - floor(rs), 
                   decreasing = diff < 0)
      ratio[idx[1:abs(diff)]] <- ratio[idx[1:abs(diff)]] + 
        switch((diff < 0) + 1, -1, 1)
    }
    
    arcs <- do.call(rbind, sapply(1:rows, function(i) {
      row_coords(row_centers[i], ratio[i])
    }, simplify = FALSE))
    
    group <- vector("numeric", sum(data$seats))
    angles <- order(atan2(arcs[, 2], arcs[, 1]))
    
    for (i in 1:length(data$seats)) {
      if (i == 1) {
        group[angles[1:data$seats[1]]] <- 1
      } else {
        group[angles[sum(c(1, data$seats[1:(i-1)])):sum(data$seats[1:i])]] <- i
      }
    }
    
    df <- data.frame(group = gl(nrow(arcs), 1), x = arcs[, 1], y = arcs[, 2], r = point_rad)

    if ("color" %in% colnames(data)) {
      df$color <- factor(group)
    }
    if ("colour" %in% colnames(data)) {
      df$colour <- factor(group)
    }
    if ("fill" %in% colnames(data)) {
      df$fill <- factor(group)
    }
    df <- StatCircle$compute_panel(df, scales, n)
    df
  }
)