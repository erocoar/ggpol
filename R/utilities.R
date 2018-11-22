"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

seq_v <- function(x) seq(nrow(x))

seq_h <- function(x) seq(ncol(x))

n_unique <- function(x) length(unique(x))

# not exported, need to recreate
# ggname <- function (prefix, grob) 
# {
#   grob$name <- grid::grobName(grob, prefix)
#   grob
# }
