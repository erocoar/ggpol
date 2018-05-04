#' Repelling text for GeomBar.
#' 
#' @importFrom ggplot2 layer
#' @export

geom_bartext <- function(mapping = NULL, 
                         data = NULL,
                         stat = "identity", 
                         position = "identity",
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         spacing = 0.005,
                         dir = "v",
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                        ...)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBartext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      spacing = spacing,
      dir = dir,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomText aes
#' @importFrom grid textGrob convertWidth grobWidth convertHeight grobHeight gpar
#' @export
GeomBartext <- ggproto("GeomBartext", GeomText,

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE, dir = "v", 
                        spacing = 0.005) {
    
    lab <- data$label
    if (parse) {
      lab <- parse(text = as.character(lab))
    }
    
    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    
    grobs <- lapply(seq_along(lab), function(x) {
      textGrob(
        lab[x],
        data$x[x], data$y[x], default.units = "native",
        hjust = data$hjust[x], vjust = data$vjust[x],
        rot = data$angle[x],
        gp = gpar(
          col = alpha(data$colour[x], data$alpha[x]),
          fontsize = data$size[x] * .pt,
          fontfamily = data$family[x],
          fontface = data$fontface[x],
          lineheight = data$lineheight[x]
        ),
        check.overlap = check_overlap
      )
    })
    
    grob_widths <- sapply(grobs, function(x) convertWidth(grobWidth(x), "npc"))
    grob_height <- convertHeight(grobHeight(grobs[[1]]), "npc", TRUE)
    
    data$overlap <- (data$ymax - data$ymin) - grob_height <= spacing
    
    if (all.equal(data$ymax, data$y)) {
      rl <- rle(data$overlap)
      rl_idx <- cumsum(rl$lengths)[rl$values] - rl$lengths[rl$values]
      data$overlap[rl_idx[rl_idx > 0]] <- TRUE
    }
    
    rl <- rle(data$overlap)
    rl$lengths <- cumsum(rl$lengths)
    
    trans <- lapply(seq_along(rl$lengths), function(x) {
    idx = seq(if (x == 1) 1 else rl$lengths[x - 1] + 1, rl$lengths[x])
    if ((rl$values[x]) & ((if (x == 1) rl$lengths[x] else (rl$lengths[x] - rl$lengths[x-1])) > 1)) {
      if (dir == "h") {
        total_width <- sum(grob_widths[idx])
        cbind("x" = seq(data$x[idx[1]] - total_width / 2,
                        data$x[idx[1]] + total_width / 2,
                        length.out = length(idx)),
              "y" = data$y[idx])
      } else {
        # extra_space <- grob_height - (data$ymax[idx] - data$ymin[idx])
        center <- if (length(idx) %% 2 == 0) data$y[idx[length(idx) %/% 2 + 1]] else {
          sum(data$y[idx[(length(idx) / 2) + c(0, 1)]]) / 2}
        out <- cbind("x" = data$x[idx],
                     "y" = seq(center - sum(grob_height * length(idx), spacing * (length(idx) - 1)) / 2,
                               center + sum(grob_height * length(idx), spacing * (length(idx) - 1)) / 2,
                               length.out = length(idx)))
        out[, 2] <- out[, 2] + (out[1, 2] < 0.03) * abs(out[1, 2])
        out
        } 
    } else {
      cbind("x" = data$x[idx], "y" = data$y[idx])
      data[idx, c("x", "y")]
      }
    })

    trans <- do.call(rbind, trans)
    data <- transform(data, x = trans[, 1], y = trans[, 2])
    
    textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  }
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]
  
  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
