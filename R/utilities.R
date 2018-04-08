#' @export
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @export
seq_v <- function(x) seq(nrow(x))

#' @export
seq_h <- function(x) seq(ncol(x))

#' @export
n_unique <- function(x) length(unique(x))