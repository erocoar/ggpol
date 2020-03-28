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
  
  labeller <- check_labeller(labeller)
  
  facets <- wrap_as_facets_list(facets)

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
#' @importFrom  rlang warn is_symbol quos_auto_name quo_is_symbol quo_is_null quo_get_expr parse_exprs parse_expr new_quosures is_string is_quosures is_quosure is_list is_formula is_bare_list flatten_if f_env expr as_quosures abort
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

#' @importFrom glue glue
check_labeller <- function(labeller) {
  labeller <- match.fun(labeller)
  is_deprecated <- all(c("variable", "value") %in% names(formals(labeller)))
  
  if (is_deprecated) {
    old_labeller <- labeller
    labeller <- function(labels) {
      Map(old_labeller, names(labels), labels)
    }
    warn(glue(
      "The labeller API has been updated. Labellers taking `variable` ",
      "and `value` arguments are now deprecated. See labellers documentation."))
  }
  
  labeller
}


wrap_as_facets_list <- function(x) {
  facets_list <- as_facets_list(x)
  compact_facets(facets_list)
}


as_facets_list <- function(x) {
  if (inherits(x, "uneval")) {
    abort("Please use `vars()` to supply facet variables")
  }
  if (is_quosures(x)) {
    x <- quos_auto_name(x)
    return(list(x))
  }
  
  # This needs to happen early because we might get a formula.
  # facet_grid() directly converted strings to a formula while
  # facet_wrap() called as.quoted(). Hence this is a little more
  # complicated for backward compatibility.
  if (is_string(x)) {
    x <- parse_expr(x)
  }
  
  # At this level formulas are coerced to lists of lists for backward
  # compatibility with facet_grid(). The LHS and RHS are treated as
  # distinct facet dimensions and `+` defines multiple facet variables
  # inside each dimension.
  if (is_formula(x)) {
    return(f_as_facets_list(x))
  }
  
  # For backward-compatibility with facet_wrap()
  if (!is_bare_list(x)) {
    x <- as_quoted(x)
  }
  
  # If we have a list there are two possibilities. We may already have
  # a proper facet spec structure. Otherwise we coerce each element
  # with as_quoted() for backward compatibility with facet_grid().
  if (is.list(x)) {
    x <- lapply(x, as_facets)
  }
  
  x
}

as_facets_list <- function(x) {
  if (inherits(x, "uneval")) {
    abort("Please use `vars()` to supply facet variables")
  }
  if (is_quosures(x)) {
    x <- quos_auto_name(x)
    return(list(x))
  }
  
  # This needs to happen early because we might get a formula.
  # facet_grid() directly converted strings to a formula while
  # facet_wrap() called as.quoted(). Hence this is a little more
  # complicated for backward compatibility.
  if (is_string(x)) {
    x <- parse_expr(x)
  }
  
  # At this level formulas are coerced to lists of lists for backward
  # compatibility with facet_grid(). The LHS and RHS are treated as
  # distinct facet dimensions and `+` defines multiple facet variables
  # inside each dimension.
  if (is_formula(x)) {
    return(f_as_facets_list(x))
  }
  
  # For backward-compatibility with facet_wrap()
  if (!is_bare_list(x)) {
    x <- as_quoted(x)
  }
  
  # If we have a list there are two possibilities. We may already have
  # a proper facet spec structure. Otherwise we coerce each element
  # with as_quoted() for backward compatibility with facet_grid().
  if (is.list(x)) {
    x <- lapply(x, as_facets)
  }
  
  x
}

# Flatten a list of quosures objects to a quosures object, and compact it
compact_facets <- function(x) {
  x <- flatten_if(x, is_list)
  null <- vapply(x, quo_is_null, logical(1))
  new_quosures(x[!null])
}

# Compatibility with plyr::as.quoted()
as_quoted <- function(x) {
  if (is.character(x)) {
    if (length(x) > 1) {
      x <- paste(x, collapse = "; ")
    }
    return(parse_exprs(x))
  }
  if (is.null(x)) {
    return(list())
  }
  if (is_formula(x)) {
    return(simplify(x))
  }
  list(x)
}
# From plyr:::as.quoted.formula
simplify <- function(x) {
  if (length(x) == 2 && is_symbol(x[[1]], "~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) {
    return(list(x))
  }
  op <- x[[1]]; a <- x[[2]]; b <- x[[3]]
  
  if (is_symbol(op, c("+", "*", "~"))) {
    c(simplify(a), simplify(b))
  } else if (is_symbol(op, "-")) {
    c(simplify(a), expr(-!!simplify(b)))
  } else {
    list(x)
  }
}

f_as_facets_list <- function(f) {
  lhs <- function(x) if (length(x) == 2) NULL else x[-3]
  rhs <- function(x) if (length(x) == 2) x else x[-2]
  
  rows <- f_as_facets(lhs(f))
  cols <- f_as_facets(rhs(f))
  
  list(rows, cols)
}

as_facets <- function(x) {
  if (is_facets(x)) {
    return(x)
  }
  
  if (is_formula(x)) {
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
  
  env <- f_env(f) %||% globalenv()
  
  # as.quoted() handles `+` specifications
  vars <- as.quoted(f)
  
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
  all(vapply(x, is_quosure, logical(1)))
}


# When evaluating variables in a facet specification, we evaluate bare
# variables and expressions slightly differently. Bare variables should
# always succeed, even if the variable doesn't exist in the data frame:
# that makes it possible to repeat data across multiple factors. But
# when evaluating an expression, you want to see any errors. That does
# mean you can't have background data when faceting by an expression,
# but that seems like a reasonable tradeoff.
#' @importFrom tibble as_tibble
eval_facets <- function(facets, data, env = globalenv()) {
  vars <- compact(lapply(facets, eval_facet, data, env = env))
  new_data_frame(as_tibble(vars))
}
eval_facet <- function(facet, data, env = emptyenv()) {
  if (quo_is_symbol(facet)) {
    facet <- as.character(quo_get_expr(facet))
    
    if (facet %in% names(data)) {
      out <- data[[facet]]
    } else {
      out <- NULL
    }
    return(out)
  }
  
  eval_tidy(facet, data, env)
}

layout_null <- function() {
  # PANEL needs to be a factor to be consistent with other facet types
  new_data_frame(list(PANEL = factor(1), ROW = 1, COL = 1, SCALE_X = 1, SCALE_Y = 1))
}

check_layout <- function(x) {
  if (all(c("PANEL", "SCALE_X", "SCALE_Y") %in% names(x))) {
    return()
  }
  
  abort("Facet layout has bad format. It must contain columns 'PANEL', 'SCALE_X', and 'SCALE_Y'")
}

as.quoted <- function (x, env = parent.frame()) 
{
  x <- if (is.character(x)) {
    lapply(x, function(x) parse(text = x)[[1]])
  }
  else if (is.formula(x)) {
    simplify_formula(x)
  }
  else if (is.call(x)) {
    as.list(x)[-1]
  }
  else {
    abort("Only knows how to quote characters, calls, and formula")
  }
  attributes(x) <- list(env = env, class = "quoted")
  x
}

compact <- function (x) 
{
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

is.formula <- function (x)  inherits(x, "formula")

new_data_frame <- function (x = list(), n = NULL) 
{
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 
      0
    else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) 
      next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}

simplify_formula <- function (x) 
{
  if (length(x) == 2 && x[[1]] == as.name("~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) 
    return(list(x))
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (op == as.name("+") || op == as.name("*") || op == as.name("~")) {
    c(simplify(a), simplify(b))
  }
  else if (op == as.name("-")) {
    c(simplify(a), bquote(-.(x), list(x = simplify(b))))
  }
  else {
    list(x)
  }
}

