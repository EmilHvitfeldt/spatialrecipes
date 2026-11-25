#' Transform geometry columns to a different CRS
#'
#' `step_spatial_transform()` creates a *specification* of a recipe step that
#' will transform one or more geometry columns to a different coordinate
#' reference system (CRS), for example from longitude and latitude to a
#' projected CRS.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which geometry columns
#'   (`sfc` columns) to transform, for example [all_geometry_predictors()]. See
#'   [recipes::selections()] for more details.
#' @param crs The CRS to transform the selected columns to. Anything accepted by
#'   [sf::st_crs()] works, such as an EPSG code (`26915`), a string
#'   (`"EPSG:26915"`), or a `crs` object. It is evaluated when the step is
#'   created and stored in the step, so it can't be tuned with [tune::tune()].
#' @param columns A character string of the selected variable names. This field
#'   is a placeholder and will be populated once [recipes::prep()] is used.
#' @param from_crs A named list of the CRS of each geometry column, recorded
#'   when [recipes::prep()] is used. This field is a placeholder and is not
#'   meant to be set by the user.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @details
#'
#' The selected columns are replaced in place and keep their names. Columns that
#' are already in the target CRS are left as they are.
#'
#' The CRS of the geometry columns is recorded in [recipes::prep()], and every
#' selected column must have one. [recipes::bake()] errors if the CRS of the new
#' data differs from the recorded one. Use [sf::st_set_crs()] to assign a CRS to
#' data that has none.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `crs`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{crs}{character, a label for the target CRS}
#'   \item{id}{character, id of this step}
#' }
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(ames, package = "modeldata")
#'
#' fixed_points <- sf::st_sfc(
#'   sf::st_point(c(-93.6460, 42.0254)),
#'   sf::st_point(c(-93.6136, 42.0256)),
#'   crs = 4326
#' )
#'
#' # Euclidean distances need a projected CRS: UTM zone 15N covers Ames
#' rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
#'   step_sfc_point(lon = Longitude, lat = Latitude) |>
#'   step_spatial_transform(all_geometry_predictors(), crs = 26915) |>
#'   step_spatial_distance(
#'     all_geometry_predictors(),
#'     ref = fixed_points,
#'     metric = "euclidean"
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' tidy(rec, number = 2)
step_spatial_transform <-
  function(
    recipe,
    ...,
    crs,
    role = NA,
    trained = FALSE,
    columns = NULL,
    from_crs = NULL,
    skip = FALSE,
    id = rand_id("spatial_transform")
  ) {
    rlang::check_required(crs)
    crs <- check_target_crs(crs)

    add_step(
      recipe,
      step_spatial_transform_new(
        terms = rlang::enquos(...),
        crs = crs,
        role = role,
        trained = trained,
        columns = columns,
        from_crs = from_crs,
        skip = skip,
        id = id
      )
    )
  }

step_spatial_transform_new <-
  function(terms, crs, role, trained, columns, from_crs, skip, id) {
    step(
      subclass = "spatial_transform",
      terms = terms,
      crs = crs,
      role = role,
      trained = trained,
      columns = columns,
      from_crs = from_crs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_spatial_transform <- function(x, training, info = NULL, ...) {
  if (isTRUE(x$trained)) {
    return(x)
  }

  columns <- recipes::recipes_eval_select(x$terms, training, info)

  from_crs <- record_crs(training, columns)
  check_has_crs(from_crs)

  step_spatial_transform_new(
    terms = x$terms,
    crs = x$crs,
    role = x$role,
    trained = TRUE,
    columns = columns,
    from_crs = from_crs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_spatial_transform <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)
  check_crs(new_data, object$from_crs)

  for (col in col_names) {
    if (!crs_equal(object$from_crs[[col]], object$crs)) {
      new_data[[col]] <- sf::st_transform(new_data[[col]], object$crs)
    }
  }
  new_data
}

#' @export
print.step_spatial_transform <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Spatial transform on "
    print_step(
      x$columns,
      x$terms,
      x$trained,
      width = width,
      title = title
    )
    invisible(x)
  }

#' @export
tidy.step_spatial_transform <- function(x, ...) {
  if (is_trained(x)) {
    terms <- unname(x$columns)
  } else {
    terms <- unname(sel2char(x$terms))
  }
  res <- tibble(
    terms = terms,
    crs = rep(crs_label(x$crs), length(terms))
  )
  res$id <- x$id
  res
}

#' @rdname required_pkgs.spatialrecipes
#' @export
required_pkgs.step_spatial_transform <- function(x, ...) {
  c("spatialrecipes", "sf")
}
