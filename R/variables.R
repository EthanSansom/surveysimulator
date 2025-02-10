# Constructors -----------------------------------------------------------------

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
    class = c(subclass, cls_nm("variable"))
  )
}

count <- function(min, max, missing_perc = 0, name = "x") {
  force(min)
  force(max)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  validator <- function(x) {
    chk::chk_whole_numeric(x, x_name = x_name)
    chk::chk_range(x, range = c(min, max), x_name = x_name)
    # TODO: Eventually we'll need more robust missing % detection to related
    # missingness between variables (i.e. via survey skip logic)
    if (missing_perc == 0) chk::chk_not_any_na(x, x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(seq(min, max), size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(seq(min, max), if (missing_perc > 0) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("count")
  )
}

range <- function(min, max, missing_perc = 0, name = "x") {
  force(min)
  force(max)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  validator <- function(x) {
    chk::chk_numeric(x, x_name = x_name)
    chk::chk_range(x, range = c(min, max), x_name = x_name)
    if (missing_perc == 0) chk::chk_not_any_na(x, x_name = x_name)
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
    if (missing_perc > 0) NA
  ))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("range")
  )
}

binary <- function(missing_perc = 0, name = "x") {
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  validator <- function(x) {
    chk::chk_subset(x, values = c(0, 1, if (missing_perc > 0) NA), x_name = x_name)
    # if (missing_perc == 0) chk::chk_not_any_na(x, x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(0:1, size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(0, 1, if (missing_perc > 0) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("binary")
  )
}

# TODO: Eventually this would be a factor with correct levels or a character
categorical <- function(values, missing_perc = 0, name = "x") {
  force(values)
  force(missing_perc)
  force(name)

  x_name <- paste0("`", name, "`") # Make `chk::chk_*()` add ticks to names
  validator <- function(x) {
    chk::chk_character(x, x_name = x_name)
    chk::chk_subset(x, values = c(values, if (missing_perc > 0) NA), x_name = x_name)
  }
  simulator <- function(size, seed) {
    set.seed(seed)
    out <- sample(values, size = size, replace = TRUE)
    simulate_missing_perc(out, missing_perc = missing_perc, seed = seed)
  }
  example <- commas(c(values, if (missing_perc > 0) NA))

  new_variable_promise(
    name = name,
    example = example,
    validator = validator,
    simulator = simulator,
    subclass = cls_nm("categorical")
  )
}

# Helpers ----------------------------------------------------------------------

simulate_missing_perc <- function(x, missing_perc = 0, seed = 1) {
  if (missing_perc <= 0) {
    return(x)
  }
  set.seed(1)
  x[runif(n = length(x)) <= missing_perc] <- NA
  x
}

commas <- function(x, n = 5) {
  x_len <- length(x)
  if (x_len <= n) {
    return(paste(x, collapse = ", "))
  }
  x <- as.character(x)
  paste(c(x[1:(n - 2)], "...", x[c(x_len - 1, x_len)]), collapse = ", ")
}

cls_nm <- function(x) {
  paste0("surveysimulator_", x)
}
