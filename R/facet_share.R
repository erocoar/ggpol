#' A shared axis for two panels
#'
#' `facet_share` uses [facet_wrap()] to build two panels with a shared axis.
#'
#' @inheritParams ggplot2::facet_wrap
#' @export
#' @param reverse_num Used when passing on flipped data (times -1) for the 
#' second (right/bottom) panel. If `TRUE`, this will multiply the axis labels for that panel by -1.
#' @export
#' @examples
#' df <- data.frame(age = sample(1:20, 1000, replace = T), gender = c("M","F"))
#' df_h <- df %>% 
#'   count(age, gender) %>% 
#'   mutate(n = ifelse(gender == "F", n * -1, n)) %>% 
#'   as.data.frame()
#' 
#' p <- ggplot(df_h, aes(x = factor(age), y = n, fill = gender)) + 
#'   geom_bar(stat = "identity") +
#'   facet_share(~gender, dir = "h", scales = "free", reverse_num = TRUE) + 
#'   coord_flip() +
#'   labs(x = "Age", y = "Count") + 
#'   theme(legend.position = "bottom")
#' 
#' p
#' 
#' # When setting direction to vertical, and if we want to mirror the second panel,
#' # we must multiply the second factor by -1.
#' # And levels(factor(gender))[2] is M. 
#' df_v <- df %>% 
#'   count(age, gender) %>% 
#'   mutate(n = ifelse(gender == "M", n*-1, n)) %>% 
#'   as.data.frame()
#'   
#' p <- ggplot(df_v, aes(x = as.factor(age), y = n, fill = gender)) + 
#'   geom_bar(stat = "identity") +   
#'   facet_share(~gender, dir = "v", reverse_num = TRUE, scales = "free", strip.position = "left") +
#'   labs(x = "Age", y = "Count") + 
#'   theme(legend.position = "left")
#' p
facet_share <- function(facets, scales = "fixed",
                        reverse_num = FALSE,
                        shrink = TRUE, labeller = "label_value", as.table = TRUE,
                        switch = NULL, drop = TRUE, dir = "h", strip.position = "top") {
  
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  reverse_num <- reverse_num
  
  free <- list(
    x = any(scales %in% c("free", "free_x")) & dir == "h",
    y = any(scales %in% c("free", "free_y")) & dir == "v"
  )
  
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right", "outer"))
  
  ggproto(NULL, FacetShare,
          shrink = shrink,
          params = list(facets = plyr::as.quoted(facets), free = free,
                        as.table = as.table, 
                        strip.position = strip.position,
                        drop = drop, 
                        labeller = labeller,
                        dir = dir,
                        reverse_num = reverse_num)
  )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid convertWidth grobWidth convertHeight grobHeight
#' @importFrom gtable gtable_matrix gtable_add_col_space gtable_add_row_space gtable_add_grob
#' @export
FacetShare <- ggproto("FacetShare", FacetWrap,
  shrink = TRUE,
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    
  panel_spacing <- switch(params$dir,
    "h" = if (is.null(theme$panel.spacing.x)) theme$panel.spacing else theme$panel.spacing.x,
    "v" = if (is.null(theme$panel.spacing.y)) theme$panel.spacing else theme$panel.spacing.y)
  
  if (params$dir == "h") {
    theme$panel.spacing.x <- unit(0, "npc")
  } else {
    theme$panel.spacing.y <- unit(0, "npc")
  }
  
  panel_table <- FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
  if (params$reverse_num) {
    if (params$dir == "h") {
      inds <- grep("axis-b|axis-t", panel_table$layout$name)
    } else {
      inds <- grep("axis-l|axis-r", panel_table$layout$name)
    }
    for (ind in inds[c(1, 3) + (params$dir == "v")]) {
      if (!"zeroGrob" %in% class(panel_table$grobs[[ind]])) {
        panel_table$grobs[[ind]]$children$axis$grobs[[1 + (params$dir == "h")]]$children[[1]]$label <-
          as.character(
            as.numeric(panel_table$grobs[[ind]]$children$axis$grobs[[1 + (params$dir == "h")]]$children[[1]]$label)*-1)
      }
    }
  }
  
  theme$panel.spacing.x <- panel_spacing
  if (params$dir == "h") {
    inds <- grep("axis-l|axis-r", panel_table$layout$name)
  } else {
    inds <- grep("axis-b|axis-t", panel_table$layout$name)
  }
   
  for (ind in inds) {
    if (!"zeroGrob" %in% class(panel_table$grobs[[ind]])) {
      panel_table$grobs[[ind]] <- zeroGrob()
    }
  }
   
  axes <- render_axes(ranges, ranges, coord, theme, 
                      transpose = TRUE)

  # compute shared axis grob
  if (params$dir == "h") {
    tick_idx <- grep("axis.ticks",
                     sapply(axes$y$left[[1]]$children$axis$grobs, function(x) x$name))
    lab_idx <- (tick_idx == 1) + 1
    
    labs <- axes$y$left[[1]]$children$axis$grobs[[lab_idx]]
    labs$children[[1]]$hjust <- 0.5
    labs$children[[1]]$x <- unit(0.5, "npc")
    
    ax_tick_l <- ax_tick_r <- axes$y$left[[1]]$children$axis$grobs[[tick_idx]]
    tick_count <- length(ax_tick_r$x)
    ax_tick_r$x[seq(1, tick_count, 2)] <- unit(0, "npc")
    ax_tick_r$x[seq(2, tick_count, 2)] <- convertWidth(grobWidth(ax_tick_l), "pt")
    
    shared_axis <- matrix(list(
      ax_tick_l,
      labs,
      ax_tick_r
    ), ncol = 3, nrow = 1)
    
    shared_axis <- gtable::gtable_matrix("shared.ax.y", shared_axis,
      widths = unit(c(axes$y$left[[1]]$children$axis$widths[[tick_idx]],
      1,
      axes$y$left[[1]]$children$axis$widths[[tick_idx]]),
      c("pt", "grobwidth", "pt"),
      list(NULL, axes$y$left[[1]]$children$axis$grobs[[lab_idx]], NULL)),
      heights = unit(1, "npc"), clip = "off")
    
    shared_axis <- gtable::gtable_add_col_space(shared_axis, panel_spacing * 1)
    
  } else {
    tick_idx <- grep("axis.ticks",
                     sapply(axes$x$bottom[[1]]$children$axis$grobs, function(x) x$name))
    lab_idx <- (tick_idx == 1) + 1
    
    labs <- axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]]
    labs$children[[1]]$vjust <- 0.4
    labs$children[[1]]$y <- unit(0.5, "npc")
    
    ax_tick_b <- ax_tick_t <- axes$x$bottom[[1]]$children$axis$grobs[[tick_idx]]
    ax_tick_t$y[seq(1, length(ax_tick_t$y), 2)] <- unit(0, "npc")
    ax_tick_t$y[seq(2, length(ax_tick_t$y), 2)] <- convertHeight(grobHeight(ax_tick_b), "pt")
    
    shared_axis <- matrix(list(
      ax_tick_b,
      labs,
      ax_tick_t
    ), ncol = 1, nrow = 3)
    
    shared_axis <- gtable::gtable_matrix("shared.ax.x", shared_axis,
      widths = unit(1, "npc"),
      heights = unit(c(axes$x$bottom[[1]]$children$axis$heights[[tick_idx]],
      1,
      axes$x$bottom[[1]]$children$axis$heights[[tick_idx]]),
      c("pt", "grobwidth", "pt"),
      list(NULL, axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]], NULL)),
      clip = "off")

    shared_axis <- gtable::gtable_add_row_space(shared_axis, panel_spacing)
  }
  
  # add shared axis
  if (params$dir == "h") {
    sa_inds <- grep("null", as.character(panel_table$widths))
    panel_table$widths[sum(sa_inds) / 2] <- convertWidth(sum(shared_axis$widths), "cm")
    panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = 4, t = 3, clip = "on")
  } else {
    sa_inds <- grep("null", as.character(panel_table$heights))
    if (sum(sa_inds) / 2 - as.integer(sum(sa_inds) / 2) != 0) {
      panel_table$heights[as.integer(sum(sa_inds) / 2)] <- convertHeight(sum(shared_axis$heights), "cm") 
      panel_table$heights[as.integer(sum(sa_inds) / 2) + 1] <- convertHeight(sum(shared_axis$heights), "cm") 
      panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = panel_table$layout$l[2], 
                                             t = as.integer(sum(sa_inds) / 2) + 1, b = 6, clip = "on")
    }
    else {
      panel_table$heights[sum(sa_inds) / 2] <- convertHeight(sum(shared_axis$heights), "cm") + 2 * panel_spacing
      panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = panel_table$layout$l[1], t = (sum(sa_inds) / 2), clip = "on")
    }
  }

  panel_table
  })