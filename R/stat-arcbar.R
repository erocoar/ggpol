#' @param n number of points used to calculate the circle polygon. Defaults to 360.
#' @param sep Separation between the individual arc components as a total proportion of pi (i.e., the entire arc)
#' @export
#' @rdname geom_arc_bar
stat_arc_bar  <- function(mapping = NULL, data = NULL, geom = "arc_bar",
                          position = "identity", n = 360, sep = 0.05, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatArcBar,
    mapping = mapping,
    data = data, 
    geom = geom,
    position = position, 
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(na.rm = na.rm, n = n, sep = sep, ...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatArcBar <- ggproto("StatArcBar", Stat,
  required_aes = c("shares", "r0", "r1"),
  
  compute_panel = function(data, scales, n = 360, sep = 0.05) {
    
    if (all(data$shares >= 1)) {
      data$shares <- data$shares / sum(data$shares)
    }
    
    if (sep > 0) {
      data$shares <- data$shares * (1 - sep)
      sep_shares <- rep(data$shares, c(rep(2, length(data$shares) - 1), 1))
      sep_shares[seq(2, (length(sep_shares) - 1), 2)] <- sep / (length(data$shares) - 1)
      
      cc <- rev(cumsum(c(0, sep_shares * pi)))
      cc[1] <- pi
      cc[length(cc)] <- 0
      cc <- rbind(cc[seq(1, length(cc), 2)], cc[seq(2, length(cc), 2)])
    } else {
      cc <- rev(cumsum(c(0, data$shares * pi)))
    }
    
    df <- lapply(1:nrow(data), function(i) {
      arc_seq <- seq(cc[1, i], cc[2, i], length.out = n)
      
      cbind(
        x = c(
          cos(arc_seq) * data$r1[i],
          rev(cos(arc_seq) * data$r0[i])
        ),
        y = c(
          sin(arc_seq) * data$r1[i],
          rev(sin(arc_seq) * data$r0[i])
        ),
        group = i
      )
    })
    
    df <- as.data.frame(do.call(rbind, df))
    
    to_add <- setdiff(colnames(data), colnames(df))
    for (col_ in to_add){
      df[, col_] <- rev(rep(data[, col_], rle(df$group)$lengths))
    }
    df
  }
)