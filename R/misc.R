# Internal helpers shared between steps go here.

record_crs <- function(data, cols, call = rlang::caller_env()) {
  out <- lapply(cols, function(col) {
    if (!col %in% names(data)) {
      cli::cli_abort(
        "Column {.field {col}} is not present in the data.",
        class = "spatialrecipes_crs_column",
        call = call
      )
    }
    if (!inherits(data[[col]], "sfc")) {
      cli::cli_abort(
        "Column {.field {col}} must be an {.cls sfc} geometry column.",
        class = "spatialrecipes_crs_column",
        call = call
      )
    }
    sf::st_crs(data[[col]])
  })
  rlang::set_names(out, cols)
}

check_crs <- function(new_data, crs, call = rlang::caller_env()) {
  if (length(crs) > 0 && !rlang::is_named(crs)) {
    cli::cli_abort("{.arg crs} must be a named list.", call = call)
  }
  for (col in names(crs)) {
    if (!inherits(new_data[[col]], "sfc")) {
      cli::cli_abort(
        "Column {.field {col}} must be an {.cls sfc} geometry column in {.arg new_data}.",
        class = "spatialrecipes_crs_column",
        call = call
      )
    }
    new_crs <- sf::st_crs(new_data[[col]])
    if (!crs_equal(new_crs, crs[[col]])) {
      cli::cli_abort(
        c(
          "The CRS of column {.field {col}} in {.arg new_data} does not match the CRS used in {.fn prep}.",
          i = "Expected: {crs_label(crs[[col]])}.",
          x = "Actual: {crs_label(new_crs)}."
        ),
        class = "spatialrecipes_crs_mismatch",
        call = call
      )
    }
  }
  invisible(new_data)
}

crs_equal <- function(x, y) {
  if (is.na(x) || is.na(y)) {
    return(is.na(x) && is.na(y))
  }
  identical(x$wkt, y$wkt) || isTRUE(x == y)
}

crs_label <- function(crs) {
  if (is.na(crs)) {
    return("no CRS")
  }
  if (!is.na(crs$input) && nchar(crs$input) <= 60) {
    return(crs$input)
  }
  name <- crs$Name
  if (!is.null(name) && !is.na(name)) {
    return(name)
  }
  "unnamed CRS"
}
