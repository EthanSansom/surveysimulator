`%notin%` <- Negate(`%in%`)

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.x, .y, FUN = .f, SIMPLIFY = FALSE, MoreArgs = rlang::list2(...))
}

map3 <- function(.x, .y, .z, .f, ...) {
  mapply(.x, .y, .z, FUN = .f, SIMPLIFY = FALSE, MoreArgs = rlang::list2(...))
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = logical(1L), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = character(1L), ...)
}

is_two_sided_formula <- function(x) {
  rlang::is_formula(x, scoped = TRUE, lhs = TRUE)
}

is_empty <- function(x) {
  length(x) == 0
}
