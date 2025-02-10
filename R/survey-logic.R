# constructors -----------------------------------------------------------------

new_survey_logic_promise <- function(
    locator,
    validator,
    enforcer,
    description,
    subclass = character()
) {
  structure(
    list(),
    locator = locator,
    validator = validator,
    enforcer = enforcer,
    description = description,
    class = c(subclass, cls_nm("survey_logic_promise"))
  )
}

# declare ----------------------------------------------------------------------

#' @export
declare_survey_logic <- function(.data, ...) {

  dots <- rlang::list2(...)
  stopifnot(is_dataset_promise(.data))
  stopifnot(map_lgl(dots, is_two_sided_formula))

  caller_env <- rlang::caller_env()

  # Generate the functions required for a <survey_logic_promise>
  survey_logic_promises <- vector("list", length(dots))
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    lhs_expr <- rlang::f_lhs(dot)
    rhs_expr <- rlang::f_rhs(dot)

    # For now, we'll just use the formula expression itself as the description
    # of a skip logic promise.
    description <- paste(rlang::expr_deparse(lhs_expr), "~", rlang::expr_deparse(rhs_expr))

    # Returns locations in `data` where the `lhs_expr` is TRUE, when evaluated
    # in the correct data-mask.
    locator <- function(data) {
      out <- rlang::eval_tidy(lhs_expr, data = data)
      out[is.na(out)] <- FALSE
      which(out)
    }

    # Define the locator function in the environment of the caller (e.g. global)
    locator <- rlang::new_function(
      args = rlang::pairlist2(data = ,),
      body = rlang::expr({
        # `rlang::eval_tidy()` expects an already captured expression
        out <- rlang::eval_tidy(quote(!!lhs_expr), data = data)
        out[is.na(out)] <- FALSE
        which(out)
      }),
      env = caller_env
    )

    # The RHS of a `declare_survey_logic()` input must be a generator
    # function with a tidy-select statement as it's input. We evaluate
    # this generator function in the caller's environment (stored in the
    # formula supplied) to get the `validator` and `enforcer` functions.
    rhs_funs <- eval(rhs_expr, envir = rlang::f_env(dot))

    survey_logic_promises[[i]] <- new_survey_logic_promise(
      locator = locator,
      validator = rhs_funs$validator,
      enforcer = rhs_funs$enforcer,
      description = description
    )
  }

  attr(.data, "survey_logic") <- append(survey_logic_promises, attr(.data, "survey_logic"))
  .data
}

# generators -------------------------------------------------------------------

# These are functions which belong on the RHS of a `...` in `declare_survey_logic()`.
# They take a tidy-select statement as input and a two functions as output.
#
# 1. One output function checks that all columns matched by the select statement
#    meet a certain condition for a given set of rows.
#
# 2. The other output function enforces the condition of the check above ^.
#
# TODO: Note, in practice, I'll want the survey skip logic to be more tightly
#       integrated with the data simulation process. For now, I'll apply the
#       skip logic conditions after data is simulated using the variable promises.

#' @export
skipped <- function(selection) {

  selection <- rlang::enexpr(selection)

  # Check whether elements in `data[rows, selection]` are all NA
  validator <- function(data, rows) {

    rows <- if (is.logical(rows)) which(rows) else rows
    cols <- tidyselect::eval_select(selection, data = data)
    data <- data[rows, cols]

    # We expect every element of `data` to be "skipped" (i.e. NA)
    if (all(is.na(data))) {
      return(invisible())
    }

    # Collect information about which columns contain non-NA values
    bullets <- c()
    for (col_nm in names(data)) {
      col <- data[[col_nm]]
      bullets <- c(
        bullets,
        expected_at_bullets(
          col = col,
          col_nm = col_nm,
          loc = rows,
          test = is.na,
          be = "NA",
          not = "non-NA"
        )
      )
    }

    # Emit the information about the non-skipped columns
    cli::cli_abort(
      message = c("Violated `skipped()` survey-logic:", bullets),
      class = cls_nm("skipped_error")
    )
  }

  # Set elements in `data[rows, selection]` to NA
  enforcer <- function(data, rows) {
    rows <- if (is.logical(rows)) which(rows) else rows
    cols <- tidyselect::eval_select(selection, data = data)
    data[rows, cols] <- NA
    data
  }

  # TODO: This should potentially be a class for validation
  list(validator = validator, enforcer = enforcer)
}

#' @export
imputed <- function(selection, value) {

  selection <- rlang::enexpr(selection)
  stopifnot(length(value) == 1)

  # Check whether elements in `data[rows, selection]` are all an imputed value
  validator <- function(data, rows) {

    rows <- if (is.logical(rows)) which(rows) else rows
    cols <- tidyselect::eval_select(selection, data = data)
    data <- data[rows, cols]

    # We expect every element of `data` to be the imputed `value`
    if (all(map_lgl(data, \(col) all(col %in% value)))) {
      return(invisible())
    }

    # Collect information about which columns contain values other than `value`
    bullets <- c()
    for (col_nm in names(data)) {
      col <- data[[col_nm]]
      bullets <- c(
        bullets,
        expected_at_bullets(
          col = col,
          col_nm = col_nm,
          loc = rows,
          test = \(x) x %in% value,
          be = paste0("be the value `", value, "`"),
          not = paste0("not the value `", value, "`")
        )
      )
    }

    # Emit the information about the non-skipped columns
    cli::cli_abort(
      message = c("Violated `imputed()` survey-logic:", bullets),
      class = cls_nm("imputed_error")
    )
  }

  # Set elements in `data[rows, selection]` to `value`
  enforcer <- function(data, rows) {
    rows <- if (is.logical(rows)) which(rows) else rows
    cols <- tidyselect::eval_select(selection, data = data)
    data[rows, cols] <- value
    data
  }

  list(validator = validator, enforcer = enforcer)
}

# helpers ----------------------------------------------------------------------

# Bullets of the form:
# i Expected column `{col_nm}` to {be} at location{?s}: {loc}
# x Column `{col_nm}` is {not} at location{?s}: {!test(col)}
# An empty character is returned if `test(col)` is always true
expected_at_bullets <- function(col, col_nm, test, loc, be, not) {

  valid_at <- test(col)
  valid_at[is.na(valid_at)] <- FALSE
  if (all(valid_at)) {
    return(character())
  }

  c(
    i = paste0("Expected column `", col_nm, "` to be ", be, " ", at_locations(loc)),
    x = paste0("Column `", col_nm, "` is ", not, " at ", at_locations(!valid_at))
  )
}
