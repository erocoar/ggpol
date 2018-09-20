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
    x = any(scales %in% c("free", "free_x")) & dir == "h",
    y = any(scales %in% c("free", "free_y")) & dir == "v"
  )

  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right", "outer"))

  if (packageVersion("ggplot2") >=  package_version("2.2.1.9000")) {
    # Flatten all facets dimensions into a single one
    facets_list <- as_facets_list(facets)
    facets <- rlang::flatten_if(facets_list, rlang::is_list)
  } else {
    facets = plyr::as.quoted(facets)
  }
  
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
#' @importFrom ggplot2 zeroGrob
#' @importFrom grid unit convertWidth grobWidth convertHeight grobHeight
#' @importFrom gtable gtable_matrix gtable_add_col_space gtable_add_row_space gtable_add_grob
#' @export
FacetShare <- ggproto("FacetShare", ggplot2::FacetWrap,
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
    
    panel_table <- ggplot2::FacetWrap$draw_panels(
      panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
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
              as.numeric(
                panel_table$grobs[[ind]]$children$axis$grobs[[1 + (
                  params$dir == "h")]]$children[[1]]$label)*-1)
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
      
      if (!"zeroGrob" %in% class(ax_tick_b)) {
        ax_tick_t$y[seq(1, length(ax_tick_t$y), 2)] <- unit(0, "npc")
        ax_tick_t$y[seq(2, length(ax_tick_t$y), 2)] <- convertHeight(grobHeight(ax_tick_b), "pt")
      }
      
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


#######
new_quosures <- function(x) {
  if (!rlang::is_list(x)) {
    stop("Expected a list of quosures")
  }
  structure(x,
            class = "quosures",
            names = rlang::names2(x)
  )
}

as_quosures <- function(x, env, named = FALSE) {
  x <- lapply(x, rlang::as_quosure, env = env)
  if (named) {
    x <- rlang::quos_auto_name(x)
  }
  new_quosures(x)
}

as_facets_list <- function(x) {
  if (inherits(x, "mapping")) {
    stop("Please use `vars()` to supply facet variables")
  }
  if (inherits(x, "quosures")) {
    x <- rlang::quos_auto_name(x)
    return(list(x))
  }
  
  # This needs to happen early because we might get a formula.
  # facet_grid() directly converted strings to a formula while
  # facet_wrap() called as.quoted(). Hence this is a little more
  # complicated for backward compatibility.
  if (rlang::is_string(x)) {
    x <- rlang::parse_expr(x)
  }
  
  # At this level formulas are coerced to lists of lists for backward
  # compatibility with facet_grid(). The LHS and RHS are treated as
  # distinct facet dimensions and `+` defines multiple facet variables
  # inside each dimension.
  if (rlang::is_formula(x)) {
    return(f_as_facets_list(x))
  }
  
  # For backward-compatibility with facet_wrap()
  if (!rlang::is_bare_list(x)) {
    x <- as_quoted(x)
  }
  
  # If we have a list there are two possibilities. We may already have
  # a proper facet spec structure. Otherwise we coerce each element
  # with as_quoted() for backward compatibility with facet_grid().
  if (is.list(x)) {
    x <- lapply(x, as_facets)
  }
  
  if (sum(vapply(x, length, integer(1))) == 0L) {
    stop("Must specify at least one variable to facet by", call. = FALSE)
  }
  
  x
}

as_quoted <- function(x) {
  if (is.character(x)) {
    if (length(x) > 1) {
      x <- paste(x, collapse = "; ")
    }
    return(rlang::parse_exprs(x))
  }
  if (is.null(x)) {
    return(list())
  }
  if (rlang::is_formula(x)) {
    return(simplify(x))
  }
  list(x)
}
# From plyr:::as.quoted.formula
simplify <- function(x) {
  if (length(x) == 2 && rlang::is_symbol(x[[1]], "~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) {
    return(list(x))
  }
  op <- x[[1]]; a <- x[[2]]; b <- x[[3]]
  
  if (rlang::is_symbol(op, c("+", "*", "~"))) {
    c(simplify(a), simplify(b))
  } else if (rlang::is_symbol(op, "-")) {
    c(simplify(a), rlang::expr(-!!simplify(b)))
  } else {
    list(x)
  }
}

f_as_facets_list <- function(f) {
  lhs <- function(x) if (length(x) == 2) NULL else x[-3]
  rhs <- function(x) if (length(x) == 2) x else x[-2]
  
  rows <- f_as_facets(lhs(f))
  cols <- f_as_facets(rhs(f))
  
  if (length(rows) + length(cols) == 0) {
    stop("Must specify at least one variable to facet by", call. = FALSE)
  }
  
  if (length(rows)) {
    list(rows, cols)
  } else {
    list(cols)
  }
}

as_facets <- function(x) {
  if (is_facets(x)) {
    return(x)
  }
  
  if (rlang::is_formula(x)) {
    # Use different formula method because plyr's does not handle the
    # environment correctly.
    f_as_facets(x)
  } else {
    vars <- as_quoted(x)
    as_quosures(vars, globalenv(), named = TRUE)
  }
}
f_as_facets <- function(f) {
  if (is.null(f)) {
    return(as_quosures(list()))
  }
  
  env <- rlang::f_env(f) %||% globalenv()
  
  # as.quoted() handles `+` specifications
  vars <- plyr::as.quoted(x=f)

  # `.` in formulas is ignored
  vars <- discard_dots(vars)
  
  as_quosures(vars, env, named = TRUE)
}

discard_dots <- function(x) {
  x[!vapply(x, identical, logical(1), as.name("."))]
}

is_facets <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (!length(x)) {
    return(FALSE)
  }
  all(vapply(x, rlang::is_quosure, logical(1)))
}


