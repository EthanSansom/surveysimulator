# constructor ------------------------------------------------------------------

#' @export
new_dataset <- function(
    variables = list(),
    survey_logic = list(),
    models = list()
  ) {

  stopifnot(sapply(variables, is_variable_promise))

  # Prepare an empty dataset which contains the variable names and types
  # corresponding to the variable promises in `variables`
  data <- tibble::tibble()
  for (variable in variables) {
    name <- attr(variable, "name")

    # All variable promise inherit a base class `surveysimulator_{type}`
    pkg_class_prefix <- cls_nm("")
    variable_type <- gsub(pkg_class_prefix, "", class(variable)[[1]])

    type <- switch(
      variable_type,
      binary = integer(),
      count = ,
      range = numeric(),
      categorical = character()
    )

    data[[name]] <- type
  }

  structure(
    data,
    variables = variables,
    survey_logic = survey_logic,
    models = models,
    class = c(cls_nm("dataset_promise"), class(data))
  )

}

#' @export
print.surveysimulator_dataset_promise <- function(x, ...) {

}

# simulate ---------------------------------------------------------------------

simulate <- function(dataset, size = 0, seed = 1) {

  variable_promises <- attr(dataset, "variables")
  variable_simulators <- map(variable_promises, attr, "simulator")
  varibale_names <- map(variable_promises, attr, "name")

  variables <- lapply()
}

# helpers ----------------------------------------------------------------------
