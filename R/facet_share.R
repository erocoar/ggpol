#' A shared axis for two panels
#'
#' `facet_share` uses [facet_wrap()] to build two panels with a shared axis.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param reverse_num Used when passing on flipped data (times -1) for the 
#' second (right/bottom) panel. If `TRUE`, this will multiply the axis labels for that panel by -1.
#' 
#' @importFrom utils packageVersion
#' @export
#' @examples
#' df <- data.frame(age = sample(1:20, 1000, replace = TRUE), 
#'                  gender = c("M","F"), levels = c("M", "F"))
#' 
#' # Get the count per age and sex
#' df$count <- 1
#' df <- aggregate(count ~ gender + age, data = df, length)
#' 
#' # For the horizontally shared axis, if we want to mirror the axes,
#' # we have to multiply the first panel by -1, and use coord_flip().
#' df_h <- df 
#' df_h$count = ifelse(df_h$gender == "F", df_h$count * -1, df_h$count)
#' 
#' p <- ggplot(df_h, aes(x = factor(age), y = count, fill = gender)) + 
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
#' df_v <- df
#' df_v$count <- ifelse(df_v$gender == "M", df_v$count * -1, df_v$count) 
#' 
#' p <- ggplot(df_v, aes(x = as.factor(age), y = count, fill = gender)) + 
#'   geom_bar(stat = "identity") +   
#'   facet_share(~gender, dir = "v", reverse_num = TRUE, 
#'               scales = "free", strip.position = "left") +
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
    x = any(scales %in% c("free", "free_x")) && dir == "h",
    y = any(scales %in% c("free", "free_y")) && dir == "v"
  )

  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right", "outer"))
  
  labeller <- ggplot2:::check_labeller(labeller)
  
  facets <- ggplot2:::wrap_as_facets_list(facets)

  ggproto(NULL, FacetShare,
          shrink = shrink,
          params = list(
            facets = facets, 
            free = free,
            as.table = as.table, 
            strip.position = strip.position,
            drop = drop, 
            labeller = labeller,
            dir = dir,
            reverse_num = reverse_num
          )
        )
}

#' @rdname ggpol-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 zeroGrob render_axes
#' @importFrom grid unit convertWidth grobWidth convertHeight grobHeight
#' @importFrom gtable gtable_matrix gtable_add_col_space gtable_add_row_space gtable_add_grob
#' @export
FacetShare <- ggproto("FacetShare", ggplot2::FacetWrap,
  shrink = TRUE,
  
  setup_data = function(data, params) {
    data
  },
  # 
  # init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
  #   scales <- Facet$init_scales(layout, x_scale = x_scale, y_scale = y_scale, params)
  #   # 
  #   if (isTRUE(params$shared_lim)) {
  #     ssx <<- scales
  #     scales
  #     abs_ranges <- sapply(scales[[names(scales)]], function(x) abs(x$range$range))
  #     scales[[names(scales)]][[1]]$range$range <- c(max(abs_ranges), min(abs_ranges)) * -1
  #     scales[[names(scales)]][[1]]$range$range <- c(min(abs_ranges), max(abs_ranges))
  #   }
  #   sos <<- scales
  #   scales
  # },
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    panel_table <- ggplot2::FacetWrap$draw_panels(
      panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    
    inds_h <- grep("axis-b|axis-t", panel_table$layout$name)
    inds_v <- grep("axis-l|axis-r", panel_table$layout$name)
    
    if (params$dir == "h") {
      inds <- inds_h
    } else {
      inds <- inds_v
    }
    
    if (isTRUE(params$reverse_num)) {
      for (ind in inds[c(1, 3) + (params$dir == "v")]) {
        if (!"zeroGrob" %in% class(panel_table$grobs[[ind]])) {
          panel_table$grobs[[ind]]$children$axis$grobs[[1 + (params$dir == "h")]]$children[[1]]$label <-
            as.character(
              as.numeric(
                panel_table$grobs[[ind]]$children$axis$grobs[[1 + (
                  params$dir == "h")]]$children[[1]]$label)*-1)
        }
      }
    }
    
    panel_spacing <- switch(
      params$dir,
      "h" = if (is.null(theme$panel.spacing.x)) theme$panel.spacing else theme$panel.spacing.x,
      "v" = if (is.null(theme$panel.spacing.y)) theme$panel.spacing else theme$panel.spacing.y)
    
    if (params$dir == "h") {
      inds <- inds_v
    } else {
      inds <- inds_h
    }
    
    theme$panel.spacing.x <- panel_spacing
    for (ind in inds) {
      if (!"zeroGrob" %in% class(panel_table$grobs[[ind]])) {
        panel_table$grobs[[ind]] <- zeroGrob()
      }
    }
    
    # lastly, we need to replace the individual axes (y or x) with our 
    # shared axis - which is essentially the same axis - except for the 
    axes <- render_axes(ranges, ranges, coord, theme, 
                        transpose = TRUE)
      
    if (params$dir == "h") {
      tick_idx <- grep(
        "polyline", #previously: axis.ticks
        sapply(axes$y$left[[1]]$children$axis$grobs, function(x) x$name))
      lab_idx <- (tick_idx == 1) + 1
      
      labs <- axes$y$left[[1]]$children$axis$grobs[[lab_idx]]
      labs$children[[1]]$hjust <- 0.5
      labs$children[[1]]$x <- unit(0.5, "npc")
      ax_tick_l <- ax_tick_r <- axes$y$left[[1]]$children$axis$grobs[[tick_idx]]
        
      if (!"zeroGrob" %in% class(ax_tick_l)) {
        tick_count <- length(ax_tick_r$x)
        ax_tick_r$x[seq(1, tick_count, 2)] <- unit(0, "npc")
        ax_tick_r$x[seq(2, tick_count, 2)] <- grid::convertWidth(grobWidth(ax_tick_l), "pt")
      }
        
      shared_axis <- matrix(list(
        ax_tick_l,
        labs,
        ax_tick_r
      ), ncol = 3, nrow = 1)
        
      shared_axis <- gtable::gtable_matrix(
        "shared.ax.y", shared_axis,
        widths = unit(c(axes$y$left[[1]]$children$axis$widths[[tick_idx]], 1,
                        axes$y$left[[1]]$children$axis$widths[[tick_idx]]),
                      c("pt", "grobwidth", "pt"),
                      list(NULL, axes$y$left[[1]]$children$axis$grobs[[lab_idx]], NULL)),
        heights = unit(1, "npc"), clip = "off")
      
      shared_axis <- gtable::gtable_add_col_space(shared_axis, panel_spacing * 1)
      } else {
        tick_idx <- grep(
          "polyline", #prev: axis.ticks
          sapply(axes$x$bottom[[1]]$children$axis$grobs, function(x) x$name))
        lab_idx <- (tick_idx == 1) + 1

        labs <- axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]]
        labs$children[[1]]$vjust <- 0.4
        labs$children[[1]]$y <- unit(0.5, "npc")

        ax_tick_b <- ax_tick_t <- axes$x$bottom[[1]]$children$axis$grobs[[tick_idx]]

        if (!"zeroGrob" %in% class(ax_tick_b)) {
          ax_tick_t$y[seq(1, length(ax_tick_t$y), 2)] <- unit(0, "npc")
          ax_tick_t$y[seq(2, length(ax_tick_t$y), 2)] <- convertHeight(grobHeight(ax_tick_b), "pt")
        }

        shared_axis <- matrix(list(
          ax_tick_b,
          labs,
          ax_tick_t
        ), ncol = 1, nrow = 3)

        shared_axis <- gtable::gtable_matrix(
          "shared.ax.x", shared_axis,
          widths = unit(1, "npc"),
          heights = unit(c(axes$x$bottom[[1]]$children$axis$heights[[tick_idx]], 1,
                           axes$x$bottom[[1]]$children$axis$heights[[tick_idx]]),
                         c("pt", "grobwidth", "pt"),
                         list(NULL, axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]], NULL)),
          clip = "off")

        shared_axis <- gtable::gtable_add_row_space(shared_axis, panel_spacing)
      }

      # add shared axis
      if (params$dir == "h") {
        sa_inds <- grep("null", as.character(panel_table$widths))
        panel_table$widths[sum(sa_inds) / 2] <- grid::convertWidth(sum(shared_axis$widths), "cm")
        panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = 4, t = 3, clip = "on")
      } else {
        if (diff(panel_table$layout$t[seq(2)]) %% 2 != 0) {
          panel_table <- gtable::gtable_add_rows(panel_table, convertHeight(
            sum(shared_axis$heights), "cm") + 2 * panel_spacing,
            ceiling(diff(panel_table$layout$t[seq(2)]) / 2))

          panel_table <- gtable::gtable_add_grob(panel_table, shared_axis,
                                                 l = panel_table$layout$l[1],
                                                 t = as.integer(diff(panel_table$layout$t[seq(2)]) / 2) + 1,
                                                 clip = "on")
        } else {
          panel_table <- gtable::gtable_add_grob(panel_table, shared_axis,
                                                 l = panel_table$layout$l[1],
                                                 t = (diff(panel_table$layout$t[seq(2)]) / 2) + 1,
                                                 clip = "on")
        }
      }
    panel_table
})