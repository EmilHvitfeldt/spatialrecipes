#' Create point geometries from longitude and latitude
#'
#' `step_sfc_point()` creates a *specification* of a recipe step that will
#' combine a longitude column and a latitude column into a single point
#' geometry column (an `sfc` column, see [sf::st_sfc()]).
#'
#' @inheritParams recipes::step_center
#' @param lon,lat Bare names of the columns holding the longitude (x) and
#'   latitude (y) coordinates. Each must select exactly one numeric column. See
#'   [recipes::selections()] for more details.
#' @param crs The coordinate reference system of the coordinates, in any form
#'   accepted by [sf::st_crs()]. Defaults to `4326`, WGS 84 longitude and
#'   latitude in degrees.
#' @param name A single string, the name of the new geometry column.
#' @param columns A named character vector with the names of the longitude and
#'   latitude columns, populated by `prep()`.
#' @param keep_original_cols A logical to keep the original longitude and
#'   latitude columns after the geometry column is created. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @details
#'
#' The new column is a plain `sfc` column in a tibble. The data does not need to
#' be an `sf` object, and the result of [recipes::bake()] is always a tibble. Use
#' [sf::st_as_sf()] on the baked data if you need an `sf` object.
#'
#' Rows where either coordinate is missing get an empty point (`POINT EMPTY`).
#'
#' The new column has the type `"other"` in the recipe, so it is not matched by
#' selectors such as [recipes::all_numeric_predictors()]. Select it by name.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `argument`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{argument}{character, either `"lon"` or `"lat"`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(ames, package = "modeldata")
#'
#' rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
#'   step_sfc_point(lon = Longitude, lat = Latitude)
#'
#' rec <- prep(rec)
#'
#' bake(rec, new_data = NULL)
#'
#' tidy(rec, number = 1)
step_sfc_point <-
  function(
    recipe,
    lon,
    lat,
    role = "predictor",
    trained = FALSE,
    crs = 4326,
    name = "geometry",
    columns = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("sfc_point")
  ) {
    check_string(name, allow_empty = FALSE)

    add_step(
      recipe,
      step_sfc_point_new(
        terms = c(lon = enquos(lon), lat = enquos(lat)),
        role = role,
        trained = trained,
        crs = crs,
        name = name,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_sfc_point_new <-
  function(
    terms,
    role,
    trained,
    crs,
    name,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "sfc_point",
      terms = terms,
      role = role,
      trained = trained,
      crs = crs,
      name = name,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sfc_point <- function(x, training, info = NULL, ...) {
  lon <- recipes::recipes_argument_select(
    x$terms["lon"],
    training,
    info,
    single = TRUE,
    arg_name = "lon"
  )
  lat <- recipes::recipes_argument_select(
    x$terms["lat"],
    training,
    info,
    single = TRUE,
    arg_name = "lat"
  )
  check_type(training[, c(lon, lat)], types = c("double", "integer"))

  if (is.na(sf::st_crs(x$crs))) {
    rlang::abort("`crs` must be a valid coordinate reference system, not `NA`.")
  }

  step_sfc_point_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    crs = x$crs,
    name = x$name,
    columns = c(lon = lon, lat = lat),
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sfc_point <- function(object, new_data, ...) {
  col_names <- unname(object$columns)
  check_new_data(col_names, object, new_data)

  coords <- data.frame(
    lon = new_data[[object$columns[["lon"]]]],
    lat = new_data[[object$columns[["lat"]]]]
  )
  geometry <- sf::st_geometry(
    sf::st_as_sf(
      coords,
      coords = c("lon", "lat"),
      crs = object$crs,
      na.fail = FALSE
    )
  )

  res <- tibble::new_tibble(
    rlang::set_names(list(geometry), object$name),
    nrow = nrow(new_data)
  )
  recipes::check_name(res, new_data, object, newname = object$name)

  new_data[[object$name]] <- geometry
  recipes::remove_original_cols(new_data, object, col_names)
}

#' @export
print.step_sfc_point <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Point geometry from "
    print_step(
      unname(x$columns),
      unname(x$terms),
      x$trained,
      width = width,
      title = title
    )
    invisible(x)
  }

#' @rdname step_sfc_point
#' @usage NULL
#' @export
tidy.step_sfc_point <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      argument = names(x$columns)
    )
  } else {
    res <- tibble(
      terms = unname(sel2char(x$terms)),
      argument = names(x$terms)
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.spatialrecipes
#' @export
required_pkgs.step_sfc_point <- function(x, ...) {
  c("spatialrecipes", "sf")
}
