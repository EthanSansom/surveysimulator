# constructor ------------------------------------------------------------------

new_derived_variable_promise <- function(
    name,
    expr,
    subclass = character()
) {
  structure(
    list(),
    name = name,
    expr = expr,
    class = c(subclass, cls_nm("variable_promise"))
  )
}

# declare ----------------------------------------------------------------------

# This is basically a deferred call to `dplyr::mutate()`
#' @export
declare_derived_variables <- function(.data, ...) {
  dots <- rlang::enexprs(...)
  promises <- map2(names(dots), dots, new_derived_variable_promise)
  attr(.data, "derived_variables") <- append(promises, attr(.data, "derived_variables"))
  .data
}
