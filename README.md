surveysimulator
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

{surveysimulator} is a package for simulating potentially-complex survey
datasets with [survey skip
logic](https://www.surveymonkey.com/product/features/survey-logic/),
other conditional survey logic, and other dependencies between
variables.

The intention of {surveysimulator} is to provide a fast and easy
mechanism to generate simulated survey data as soon as a survey
instrument has been designed. Instead of waiting for survey data to
arrive to begin your analysis, you can immediately start writing code to
clean and analyze your anticipated data using a simulated dataset of the
correct structure.

Once your survey data *does* arrive, {surveysimulator} provides
functions for validating your actual data against the expected data that
you’ve simulated, allowing data-collection errors to be surfaced
quickly.

## Installation

⚠️ This package is still under construction. ⚠️

You can install the development version of {surveysimulator} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/surveysimulator")
```

## Features

The `<dataset_promise>` class is a the back-bone of {surveysimulator}.
As the name suggests, a `<dataset_promise>` is the promise of a dataset
with a given structure.

``` r
library(dplyr, warn.conflicts = FALSE)
library(surveysimulator, warn.conflicts = FALSE)

# Create a <dataset_promise> using `new_dataset()`
dataset <- new_dataset()
print(dataset)
#> <dataset_promise[?? x 0]>
```

{survey_simulator} provides the following verbs which, using a familiar
{dplyr}-like syntax, allow you to specify the structure of your
simulated dataset.

- `declare_raw_variables()`, declare the names and types of variables in
  the dataset
- `declare_derived_variables()`, declare the names and formulation of
  variables composed of raw variables
- `declare_survey_logic()`, declare relationships between variables
  enforced by survey logic
- `declare_relationships()`, declare expected relationships between
  variables (e.g. clusters, correlations). This is not implemented.

Catchier names for these verbs (and this package) are pending further
development. Until then, observe their use below.

``` r
dataset <- dataset |>

  # Declare the variables to simulate, specifying their name and type
  declare_raw_variables(
    # `edu_years` is a whole number 0, 1, ..., 30, where the upper
    # bound of 30 is a limit set by the electronic survey.
    edu_yrs = count(min = 0, max = 30),
    employed = binary(missing_perc = 0.1),
    work_hrs = range(min = 0, max = 60),
    pay_type = categorical(c("wage", "salary")),
    wage_amnt = range(min = 0, max = 1000),
    salary_amnt = range(min = 0, max = 500*1000)
  ) |>

  # Declare variables derived from others
  declare_derived_variables(
    income_amnt = dplyr::case_match(
        pay_type,
        "wage" ~ wage_amnt,
        "salary" ~ salary_amnt
    )
  ) |>

  # Declare dependencies between the variables arising from survey logic
  declare_survey_logic(
    # If `employed` is missing (i.e. unanswered) or the
    # respondent is un-employed, then all other questions should 
    # have been skipped (i.e. NA)
    is.na(employed) | employed == 0 ~ skipped(-c(edu_yrs, employed)),
    
    # If `pay_type` is missing, then `*_amnt` questions
    # should have been skipped
    is.na(pay_type) ~ skipped(ends_with("_amnt")),
     
    # If `pay_type` is "wage", `salary_amnt` should have an
    # imputed value of `0`. Similar for `wage_amnt`.
    pay_type == "wage" ~ imputed(salary_amnt, 0),
    pay_type == "salary" ~ imputed(wage_amnt, 0)
  )
```

Our `dataset` now promises to be composed of the variables declared
above.

``` r
print(dataset)
#> <dataset_promise[?? x 7]>
#> # Variables: 
#> • edu_yrs <count>        e.g. 0, 1, 2, ..., 30, NA
#> • employed <binary>      e.g. 0, 1, NA
#> • work_hrs <range>       e.g. 57.04, 17.35, 56.67, 43.26, NA
#> • pay_type <categorical> e.g. "wage", "salary", NA
#> • wage_amnt <range>      e.g. 517.54, 574.28, 741.44, 89.05, NA
#> • salary_amnt <range>    e.g. 11000.35, 69717.3, 485879.15, 458410.65, NA
#> • income_amnt <derived>
#> # Survey Logic: 
#> • is.na(employed) | employed == 0 ~ skipped(-c(edu_yrs, employed))
#> • is.na(pay_type) ~ skipped(ends_with("_amnt"))
#> • pay_type == "wage" ~ imputed(salary_amnt, 0)
#> • pay_type == "salary" ~ imputed(wage_amnt, 0)
```

We can make good on this promise using `simulate()`, which creates a
dataset abiding by the rules we’ve set using the `declare_*()` family of
functions.

``` r
simulated <- simulate(dataset, size = 10, seed = 123)
print(simulated)
#> # A tibble: 10 × 7
#>    edu_yrs employed work_hrs pay_type wage_amnt salary_amnt income_amnt
#>      <int>    <int>    <dbl> <chr>        <dbl>       <dbl>       <dbl>
#>  1      30        1    57.8  wage          45.8          0         45.8
#>  2      14        1    54.1  salary         0        47420.     47420. 
#>  3      18        1    41.4  salary         0       191985.    191985. 
#>  4      13        0    NA    <NA>          NA           NA         NA  
#>  5       2        1     1.48 wage         561.           0        561. 
#>  6       9        0    NA    <NA>          NA           NA         NA  
#>  7      17        1    45.5  wage         128.           0        128. 
#>  8      21        0    NA    <NA>          NA           NA         NA  
#>  9      10        0    NA    <NA>          NA           NA         NA  
#> 10       4        0    NA    <NA>          NA           NA         NA
```

Along with simulation, we get data-validation for free via the
`assert()` function. `assert()` checks that an `actual` dataset has the
same structure as an `expected` dataset promise.

``` r
assert(actual = simulated, expected = dataset)
#> ✔ `actual` dataset is as expected!
```

Datasets created using `simulate()` will pass this assertion by
definition. `assert()` is most useful for verifying that the raw data
you’ve received matches your expectations.

``` r
raw_data <- tibble::tibble( # Missing the `income_amnt` column
  edu_yrs = c(0, 1, 2, 3, 4),
    employed = c(1, 1, NA, NA, 1),
    work_hrs = c(5, 10, 1, NA, 20), # Should be NA when `employed` is NA
    pay_type = c("temp", "wage", NA, NA, "salary"), # "temp" not allowed
    wage_amnt = c(10, 20, NA, NA, 0),
    salary_amnt = c(0, 0, NA, NA, 50000)
)
assert(actual = raw_data, expected = dataset)
#> ✖ `actual` dataset contained unexpected features.
#> 
#> ── Dataset ──
#> 
#> ✖ `actual` data is missing expected columns: "income_amnt"
#> 
#> ── Columns ──
#> 
#> ✖ `pay_type` must have values matching 'salary', 'wage' or 'NA'.
#> 
#> ── Survey Logic ──
#> 
#> ! Violated `skipped()` survey-logic:
#> ℹ Expected column `work_hrs` to be NA at 2 locations: 3, 4
#> ✖ Column `work_hrs` is non-NA at the location: 3
```

`assert()` currently relies on the
[chk](https://poissonconsulting.github.io/chk/) package as the back-end
for validation, but custom validation will be used in the future.

## Planned Features

This prototype simulates all variables as independent and uniform. The
intention is to support the simulation of a mixed design and correlated
variables.
[This](https://debruine.github.io/faux/articles/sim_mixed.html), and
other useful features, are already implemented in Lisa DeBruine’s
[{faux}](https://debruine.github.io/faux/index.html) package.

I’m not looking to re-invent the wheel here, so some/most of the
simulation in {surveysimulator} will be powered by {faux} or will be
implemented following this [paper](https://osf.io/3cz2e/) by Lisa
DeBruine and Dale J. Barr which is linked in {faux}’s documentation.

I do hope to implement some features which are useful for simulating
survey data, in particular:

- Simulating various [forms of
  missingness](https://www.ncbi.nlm.nih.gov/books/NBK493614/) (e.g.
  MCAR, MAR, MNAR)
- Simulating truncated distributions, which can potentially arise from
  the limiting of electronic survey responses to a fixed range (e.g.
  hourly wages between \$0 - \$1000)
- Simulating extreme outliers, which can arise from typos (or loss of
  interest in the survey)
- Simulating longitudinal data (i.e. repeated surveys), with
  differential attrition

Additionally, I’d be curious to see if there exists survey-design
literature on different kinds of survey takers and how to identify them.
For example a non-compliant-non-dropout respondent, who diligently
responds to electronic surveys but mashes the first multiple-choice
option or default value of every question. If so, {surveysimulator} may
be able to insert such respondents into the simulated dataset.
[Here](https://pmc.ncbi.nlm.nih.gov/articles/PMC6729115/) is a
potentially related paper detecting phases of respondent drop-out.

*At the end of the day* I am admittedly prone to scope creep and so
would like to limit the reach of this package to *simulating survey data
with conditional logic and missing responses* via a familiar
{dplyr}-like interface.
