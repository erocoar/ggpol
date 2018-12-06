#' Confusion Matrix
#'
#' Plot a confusion matrix.
#' @name GeomConfmat
#' @inheritParams ggplot2::geom_tile
#' @param width The tile height.
#' @param height The tile width.

#' @param annotate Boolean indicator for whether to include values as text in each tile.
#' @param text.perc Boolean indicator for whether to add `\%` to the end of each value.
#' @param text.digits Numeric vector equal to length one, indicating the number of digits to display. `round(x, text.digits)`.
#' @param text.alpha Alpha value for the text grobs.
#' @param text.angle Angle value for the text grobs.
#' @param text.colour Colour of the text grobs.
#' @param text.family Font family of the text grobs.
#' @param text.fontface Font face of the text grobs.
#' @param text.group Groups of the text grobs.
#' @param text.hjust Hjust of the text grobs.
#' @param text.lineheight Lineheight of the text grobs.
#' @param text.size Size of the text grobs.
#' @param text.vjust Vjust of the text grobs.
#' 
#' @examples
#' x <- sample(LETTERS[seq(4)], 50, replace = TRUE)
#' y <- sample(LETTERS[seq(4)], 50, replace = TRUE)
#' 
#' ggplot() + 
#'   geom_confmat(aes(x = x, y = y), normalize = TRUE, text.perc = TRUE)
#' 
#' 
#' @importFrom ggplot2 layer 
#' @export
geom_confmat <- function(
  mapping = NULL, 
  data = NULL,
  stat = "confmat", 
  position = "identity",
  width = NULL, 
  height = NULL, 
  annotate = TRUE,
  text.perc = FALSE, 
  text.digits = 3,
  text.alpha = NULL,
  text.angle = NULL,
  text.colour = NULL,
  text.family = NULL,
  text.fontface = NULL,
  text.group = NULL,
  text.hjust = NULL,
  text.lineheight = NULL,
  text.size = NULL,
  text.vjust = NULL,
  ..., 
  na.rm = FALSE, 
  show.legend = NA,
  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomConfmat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      height = height,
      annotate = annotate,
      text.perc = text.perc,
      text.digits = text.digits,
      text.alpha = text.alpha,
      text.angle = text.angle,
      text.colour = text.colour,
      text.family = text.family,
      text.fontface = text.fontface,
      text.group = text.group,
      text.hjust = text.hjust,
      text.lineheight = text.lineheight,
      text.size = text.size,
      text.vjust = text.vjust,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 GeomTile ggproto GeomText
#' @importFrom grid grobTree
#' @importFrom rlang eval_tidy
#' @importFrom plyr empty quickdf
#' @export
GeomConfmat <- ggproto("GeomConfmat", GeomTile,
  extra_params = c("na.rm", "width", "height"),
  
  default_aes = aes(colour = "black", size = 0.1, linetype = 1,
                   alpha = NA, fill = stat(Freq)),
  
  required_aes = c("x", "y"),
  
  draw_panel = function(data, panel_params, coord, 
                       annotate, text.perc, text.digits,
                       text.alpha, text.angle, text.colour, text.family,
                       text.fontface, text.group, text.hjust,
                       text.lineheight, text.size, text.vjust) {
   if (isTRUE(annotate)) {
     text_df <- data.frame(
       x = data$x,
       y = data$y,
       alpha = text.alpha %||% data$alpha %||% 1,
       colour = text.colour %||% data$color[1] %||% "black",
       group = text.group %||% data$group,
       stringsAsFactors = FALSE
     )
     
     if (isTRUE(text.perc)) {
       label <- data$Freq * 100
       label <- if (!is.null(text.digits)) round(label, text.digits) else label
       label <- paste0(as.character(label), "%")
     } else {
       if (!is.null(text.digits)) {
         label <- as.character(round(data$Freq, text.digits))
       } else {
         label <- as.character(data$Freq)
       }
     }
     text_df$label <- label
     missing_aes <- setdiff(names(GeomText$default_aes), names(text_df))
     missing_eval <- lapply(GeomText$default_aes[missing_aes], rlang::eval_tidy)
     if (plyr::empty(text_df)) {
       data <- plyr::quickdf(missing_eval)
     } else {
       text_df[missing_aes] <- missing_eval
     }
     
     text_df$family <- text.family %||% text_df$family
     text_df$fontface <- text.fontface %||% text_df$fontface
     text_df$lineheight <- text.lineheight %||% text_df$lineheight
     text_df$hjust <- text.hjust %||% text_df$hjust
     text_df$vjust <- text.vjust %||% text_df$vjust
     text_df$size <- text.size %||% text_df$size
     
     text_grob <- GeomText$draw_panel(text_df, panel_params, coord)
   } else {
     text_grob <- NULL
   }
   
    tree <- grobTree(GeomRect$draw_panel(data, panel_params, coord),
                     text_grob)
    tree$name <- grid::grobName(tree, "geom_confmat")
    tree
   # ggname("geom_confmat", grobTree(
   #   GeomRect$draw_panel(data, panel_params, coord),
   #   text_grob
   # ))
  }
)