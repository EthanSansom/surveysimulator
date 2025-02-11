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

at_locations <- function(locations, n = 5) {
  if (is.logical(locations)) {
    locations <- which(locations)
  } else {
    locations <- sort(unique(locations))
  }
  switch(
    as.character(length(locations)),
    `0` = "at no locations",
    `1` = paste("at the location:", locations),
    paste("at", length(locations), "locations:", commas(locations, n = n))
  )
}
