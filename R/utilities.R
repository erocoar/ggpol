"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

seq_v <- function(x) seq(nrow(x))