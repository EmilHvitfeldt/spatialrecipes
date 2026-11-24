#' Select geometry columns
#'
#' `all_geometry()` selects all [`sfc`][sf::st_sfc()] columns, regardless of
#' role. `all_geometry_predictors()` selects all `sfc` columns that have the
#' `"predictor"` role.
#'
#' recipes types an `sfc` column as `"other"`, so selectors such as
#' [recipes::all_numeric_predictors()] never match it. These selectors match on
#' the `sfc` class instead, so unlike `has_type("other")` they do not match
#' other list-columns.
#'
#' Loading spatialrecipes registers the data type `"sfc"` for `sfc` columns, so
#' `has_type("other")` no longer matches them in any recipe.
#'
#' @return A selector function for use in recipe steps.
#' @seealso [recipes::has_role()], [recipes::has_type()]
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(ames, package = "modeldata")
#'
#' rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
#'   step_sfc_point(lon = Longitude, lat = Latitude) |>
#'   prep()
#'
#' recipes_eval_select(
#'   rlang::quos(all_geometry_predictors()),
#'   data = bake(rec, new_data = NULL),
#'   info = summary(rec)
#' )
all_geometry <- function() {
  recipes::has_type("sfc")
}

#' @rdname all_geometry
#' @export
all_geometry_predictors <- function() {
  intersect(recipes::has_role("predictor"), recipes::has_type("sfc"))
}

#' @exportS3Method recipes::.get_data_types
.get_data_types.sfc <- function(x) {
  "sfc"
}
