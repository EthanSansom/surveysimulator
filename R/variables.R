# constructors -----------------------------------------------------------------

new_variable_promise <- function(
    name,
    example,
    validator,
    simulator,
    subclass = character()
) {
  structure(
    list(),
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    class = c(subclass, cls_nm("variable_promise"))
  )
}

#' @export
count <- function(min, max, missing_perc = NA, name = "x") {
  force(min)
  force(max)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  no_missing <- isTRUE(missing_perc == 0)

  validator <- function(x) {
    chk::chk_whole_numeric(x, x_name = x_name)
    chk::chk_range(x, range = c(min, max), x_name = x_name)
    # TODO: Eventually we'll need more robust missing % detection to related
    # missingness between variables (i.e. via survey skip logic)
    if (no_missing) chk::chk_not_any_na(x, x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(seq(min, max), size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(seq(min, max), if (!no_missing) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("count")
  )
}

#' @export
range <- function(min, max, missing_perc = NA, name = "x") {
  force(min)
  force(max)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  no_missing <- isTRUE(missing_perc == 0)

  validator <- function(x) {
    chk::chk_numeric(x, x_name = x_name)
    chk::chk_range(x, range = c(min, max), x_name = x_name)
    if (no_missing) chk::chk_not_any_na(x, x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    # TODO: We'd eventually want to allow these variable promises to accept
    # a custom distribution function.
    out <- runif(size, min = min, max = max)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(
    round(runif(4, min = min, max = max), 2),
    if (!no_missing) NA
  ))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("range")
  )
}

#' @export
binary <- function(missing_perc = NA, name = "x") {
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  no_missing <- isTRUE(missing_perc == 0)

  validator <- function(x) {
    chk::chk_subset(x, values = c(0, 1, if (!no_missing) NA), x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(c(0L, 1L), size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(0L, 1L, if (no_missing) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("binary")
  )
}

# TODO: Eventually this would be a factor with correct levels or a character
#' @export
categorical <- function(values, missing_perc = NA, name = "x") {
  force(values)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  no_missing <- isTRUE(missing_perc == 0)

  validator <- function(x) {
    chk::chk_character(x, x_name = x_name)
    chk::chk_subset(x, values = c(values, if (!no_missing) NA), x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(values, size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(encodeString(values, quote = '"'), if (!no_missing) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("categorical")
  )
}

# predicates -------------------------------------------------------------------

#' @export
is_variable_promise <- function(x) {
  inherits(x, cls_nm("variable_promise"))
}

# declare ----------------------------------------------------------------------

#' @export
declare_raw_variables <- function(.data, ...) {

  dots <- rlang::enexprs(...)
  dot_names <- names(dots)
  stopifnot(is_dataset_promise(.data))
  caller_env <- rlang::caller_env()

  # Generate the <variable_promise> objects
  variable_promises <- vector("list", length(dots))
  for (i in seq_along(dots)) {
    # The name of the `..i` argument is the name of our variable
    name <- dot_names[[i]]

    # The value of `..i` is an un-evaluated variable promise generator. We
    # feed the RHS call the correct name and evaluate
    generator <- rlang::call_modify(dots[[i]], name = name)

    variable_promises[[i]] <- eval(generator, envir = caller_env)
  }

  # TODO: Support the over-writing of old variables, currently we'll reach an
  #       error if you try to do so.
  attr(.data, "variables") <- append(variable_promises, attr(.data, "variables"))
  .data
}

# helpers ----------------------------------------------------------------------

# TODO: Eventually we'll want to model not-at-random missingness
simulate_missing_perc <- function(x, missing_perc = 0, seed = 1) {
  if (is.na(missing_perc) || missing_perc <= 0) {
    return(x)
  }
  set.seed(1)
  x[runif(n = length(x)) <= missing_perc] <- NA
  x
}
