
# list constructor
cons <- function (x, xs) {
  unlist(list (list(x), xs), recursive = F)
}

# list constructor
snoc <- function (xs, x) {
  unlist(list (xs, list(x)), recursive = F)
}
