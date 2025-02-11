# todos ------------------------------------------------------------------------

#### Custom Validation for `assert()`
#
# TLDR: In this comment I go back and forth between using the {pointblank}
# package for validation. While I would like to generate some pointblank-esque
# HTML reports of data quality using g-table, reactable, or other, the focus
# of *this* package is simulation, not validation. I think the way to go is to
# keep things simple, with custom validation functions and a way to specify a
# general `variable()` promise, onto which the user can add their own validation.
#
# We could consider using {pointblank} to do these assertions, since their
# suite is so huge. See their set of validation functions:
# https://rstudio.github.io/pointblank/reference/index.html#validation-expectation-and-test-functions
#
# You can still create custom expectations as needed using `specially()`:
# https://rstudio.github.io/pointblank/reference/specially.html
#
# I think {pointblank} is the way to go. If the dataset promise was able to produce
# an `agent` or `informant`, then that could be built on using arbitrary functions.
# We should also support the addition of arbitrary {pointblank} validation tests
# in the dataset promise object.
#
# Then again, it might be more fun to spin up your own HTML generation
# using reactable, g-table, or similar. It might be better to make this compatible
# with other general and simple check packages and keep things simple. Also, this
# package really *isn't* about validation, it's about survey data simulation. The
# validation part is a happy by-product of the leg-work required to simulate the
# correct variables.

# constructor ------------------------------------------------------------------

#' @export
new_dataset <- function(
    variables = list(),
    derived_variables = list(),
    survey_logic = list(),
    models = list()
  ) {

  # TODO: Better validation everywhere
  stopifnot(map_lgl(variables, is_variable_promise))

  structure(
    data,
    variables = variables,
    derived_variables = derived_variables,
    survey_logic = survey_logic,
    models = models,
    class = c(cls_nm("dataset_promise"), class(data))
  )

}

#' @export
print.surveysimulator_dataset_promise <- function(x, ...) {

  variables <- attr(x, "variables")
  bullets_var <- character(length(variables))
  examples <- character(length(variables))

  # Gather bullets corresponding to the <variable_promise>'s
  for (i in seq_along(variables)) {
    # All variable promise inherit a base class `surveysimulator_{type}`
    variable <- variables[[i]]
    type <- gsub(cls_nm(""), "", class(variable)[[1]])
    name <- attr(variable, "name")
    example <- attr(variables[[i]], "example")

    bullets_var[[i]] <- paste0(name, " <", type, ">")
    examples[[i]] <- paste("e.g.", example)
  }

  if (!is_empty(bullets_var)) {
    bullets_var <- format(bullets_var, width = max(nchar(bullets_var)), justify = "left")
    bullets_var <- paste(bullets_var, examples)
  }

  # Gather bullets corresponding to derived variables
  derived_variables <- attr(x, "derived_variables")
  if (!is_empty(derived_variables)) {
    bullets_dvar <- paste(map_chr(derived_variables, attr, "name"), "<derived>")
  } else {
    bullets_dvar <- character()
  }

  # Gather bullets corresponding to the <survey_logic_promises>
  survey_logic <- attr(x, "survey_logic")
  bullets_svy <- map_chr(survey_logic, attr, "description")

  # `cli_bullets()` trims whitespace, which I don't want for bullets
  cli::cat_line(paste0("<dataset_promise[?? x ", length(variables) + length(derived_variables), "]>"))
  if (!is_empty(bullets_var) | !is_empty(bullets_dvar)) {
    cli::cat_line("# Variables: ")
    cli::cat_line(paste(cli::symbol$bullet, bullets_var))
    cli::cat_line(paste(cli::symbol$bullet, bullets_dvar))
  }
  if (!is_empty(bullets_svy)) {
    cli::cat_line("# Survey Logic: ")
    cli::cat_line(paste(cli::symbol$bullet, bullets_svy))
  }
}

# simulate ---------------------------------------------------------------------

#' @export
simulate <- function(dataset, size = 0, seed = 1) {

  caller_env <- rlang::caller_env()
  set.seed(seed)

  # Generate the variables
  variable_promises <- attr(dataset, "variables")
  variable_simulators <- map(variable_promises, attr, "simulator")
  variable_names <- map(variable_promises, attr, "name")

  variables <- map(variable_simulators, \(sim) sim(size = size, seed = seed))
  out <- tibble::tibble(!!!rlang::set_names(variables, variable_names))

  # Enforce the survey logic
  survey_logic_promises <- attr(dataset, "survey_logic")
  logic_locators <- map(survey_logic_promises, attr, "locator")
  logic_enforcers <- map(survey_logic_promises, attr, "enforcer")

  for (i in seq_along(survey_logic_promises)) {
    locator <- logic_locators[[i]]
    enforcer <- logic_enforcers[[i]]
    out <- enforcer(data = out, rows = locator(out))
  }

  # Create the derived variables. It's important that this happens after
  # survey logic has been enforced on the raw `variables`.
  derived_variable_promises <- attr(dataset, "derived_variables")
  derived_names <- map(derived_variable_promises, attr, "name")
  derived_exprs <- map(derived_variable_promises, attr, "expr")

  for (i in seq_along(derived_variable_promises)) {
    name <- derived_names[[i]]
    expr <- derived_exprs[[i]]
    out[[name]] <- rlang::eval_tidy(expr, data = out, env = caller_env)
  }

  out
}

# assert -----------------------------------------------------------------------

# TODO: We can streamline this a lot by having custom `validator()` functions
# which don't emit an error, but do emit a pass or fail message (wrapped in
# a custom class). Then, the caller of `assert()` can decide if they want pass
# and fail messages, plus they can specify how verbose the output should be.

#' @export
assert <- function(actual, expected) {

  caller_env <- rlang::caller_env()

  variable_promises <- attr(expected, "variables")
  variable_validators <- map(variable_promises, attr, "validator")
  variable_names <- map_chr(variable_promises, attr, "name")

  derived_variable_promises <- attr(expected, "derived_variables")
  derived_variable_exprs <- map(derived_variable_promises, attr, "expr")
  derived_variable_names <- map_chr(derived_variable_promises, attr, "name")

  ## Assert data-wide conditions first
  data_wide_bullets <- c()

  # Column names
  expected_col_nms <- c(variable_names, derived_variable_names)
  actual_col_nms <- names(actual)
  if (!setequal(expected_col_nms, actual_col_nms)) {
    missing_cols <- setdiff(expected_col_nms, actual_col_nms)
    extra_cols <- setdiff(actual_col_nms, expected_col_nms)

    data_wide_bullets <- c(
      data_wide_bullets,
      if (!is_empty(missing_cols)) {
        c(x = paste(
          "`actual` data is missing expected columns:",
          commas(encodeString(missing_cols, quote = '"'))
        ))
      },
      if (!is_empty(extra_cols)) {
        c(x = paste(
          "`actual` data is contains unexpected columns:",
          commas(encodeString(extra_cols, quote = '"'))
        ))
      }
    )
  }

  ## Assert column-specific conditions
  column_bullets <- c()

  # We'll use the `validator()` of each variable promise in the `expected` dataset,
  # only for those columns which are in the `actual` data.
  shared_variable_names <- intersect(variable_names, actual_col_nms)
  for (name in shared_variable_names) {
    i <- which(name == variable_names)
    validator <- variable_validators[[i]]
    # TODO: Since we're currently using `chk::chk_*()` functions for validation,
    # we can't just retrieve the error message. For now, attributing any error to
    # a failed check, will use custom validation later.
    bullet <- rlang::try_fetch(
      {
        validator(actual[[name]])
        character()
      },
      error = function(cnd) {
        rlang::set_names(c(cnd$message, cnd$body), "x")
      }
    )
    column_bullets <- c(column_bullets, bullet)
  }

  # Here we create what the derived variable *should* be and compare those values
  # to what it actually is.
  shared_derived_variable_names <- intersect(derived_variable_names, actual_col_nms)
  for (name in shared_derived_variable_names) {
    i <- which(name == derived_variable_names)
    expr <- derived_variable_exprs[[i]]

    actual_col <- actual[[name]]
    expected_col <- rlang::eval_tidy(expr, data = actual, env = caller_env)
    unexpected_at <- !as.logical(map2(actual_col, expected_col, identical))
    if (any(unexpected_at)) {
      column_bullets <- c(
        column_bullets,
        paste0(
          "Derived column `", name, "` contains unexpected values ",
          at_locations(unexpected_at)
        )
      )
    }
  }

  ## Assert survey logic

  survey_logic_promises <- attr(expected, "survey_logic")
  survey_logic_locators <- map(survey_logic_promises, attr, "locator")
  survey_logic_validators <- map(survey_logic_promises, attr, "validator")
  survey_logic_descriptions <- map(survey_logic_promises, attr, "description")

  survey_logic_bullets <- c()
  for (i in seq_along(survey_logic_promises)) {
    locator <- survey_logic_locators[[i]]
    validator <- survey_logic_validators[[i]]
    description <- survey_logic_descriptions[[i]]

    # The `locator()` function expression might not be valid (i.e. if we're
    # missing columns in `actual`), so on error we just say that we can't check
    # the skip logic condition.
    rows <- rlang::try_fetch(locator(data = actual), error = function(cnd) NULL)
    if (is.null(rows)) {
      bullet <- c(x = paste0("Can't evaluate survey logic: `", description, "`"))
    }
    else {
      # TODO: Once again, it would be preferable to have the `validator()` just
      # return it's message in this case.
      bullet <- rlang::try_fetch(
        {
          validator(data = actual, rows = rows)
          character()
        },
        error = function(cnd) {
          c(`!` = cnd$message, cnd$body)
        }
      )
    }
    survey_logic_bullets <- c(bullet, survey_logic_bullets)
  }

  ## Emit assertion error

  # If all of our bullets are empty, there is no error. In the future, we may
  # want to have `validator()` functions emit an affirmative message on a pass,
  # so we can report all tests (passing and failing) to the caller.

  data_wide_errors <- !is_empty(data_wide_bullets)
  column_errors <- !is_empty(column_bullets)
  survey_logic_errors <- !is_empty(survey_logic_bullets)

  if (!data_wide_errors && !column_errors && !survey_logic_errors) {
    cli::cli_alert_success("`actual` dataset is as expected!")
    return(invisible(actual))
  }

  # Emit an error and add formatted `cli` failure messages.

  # TODO: For the demo I'm throwing a message rather than an error, until
  #       we format the error messages in a friendly-er way.
  if (FALSE) {
    on.exit(
      {
        if (data_wide_errors) {
          cli::cli_h2("Dataset")
          cli::cli_bullets(data_wide_bullets)
        }
        if (column_errors) {
          cli::cli_h2("Columns")
          cli::cli_bullets(column_bullets)
        }
        if (survey_logic_errors) {
          cli::cli_h2("Survey Logic")
          cli::cli_bullets(survey_logic_bullets)
        }
      },
      add = TRUE
    )
    cli::cli_abort("`actual` dataset contained unexpected features.")
  }

  cli::cli_alert_danger("`actual` dataset contained unexpected features.")
  if (data_wide_errors) {
    cli::cli_h2("Dataset")
    cli::cli_bullets(data_wide_bullets)
  }
  if (column_errors) {
    cli::cli_h2("Columns")
    cli::cli_bullets(column_bullets)
  }
  if (survey_logic_errors) {
    cli::cli_h2("Survey Logic")
    cli::cli_bullets(survey_logic_bullets)
  }
  invisible(actual)
}

# predicates -------------------------------------------------------------------

is_dataset_promise <- function(x) {
  inherits(x, cls_nm("dataset_promise"))
}
