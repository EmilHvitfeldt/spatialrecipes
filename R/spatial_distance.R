#' Distance to a reference location
#'
#' `step_spatial_distance()` creates a *specification* of a recipe step that
#' will compute the distance from each point in one or more geometry columns to
#' a reference: a fixed set of points (`ref`), or a second geometry column in
#' the data (`ref_col`).
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which geometry columns
#'   (`sfc` columns of points) to compute distances from, for example
#'   [all_geometry_predictors()]. See [recipes::selections()] for more details.
#' @param ref An `sfc` or `sf` object of points to measure distances to. It is
#'   evaluated when the step is created and stored in the step. It is
#'   transformed to the CRS of each selected column when the recipe is trained.
#'   Exactly one of `ref` and `ref_col` must be given.
#' @param ref_col Bare name of a second geometry column in the data to measure
#'   distances to, row by row. It must select exactly one `sfc` column with the
#'   same CRS as the columns in `...`, and is never itself used as a column to
#'   measure from. Exactly one of `ref` and `ref_col` must be given.
#' @param metric A single string, one of `"haversine"`, `"euclidean"` or
#'   `"manhattan"`. `"haversine"` measures great-circle distance on a sphere
#'   with a radius of 6371008.8 meters, requires longitude and latitude
#'   coordinates, and returns meters. `"euclidean"` and `"manhattan"` measure
#'   in the units of a projected (or missing) CRS and error on longitude and
#'   latitude coordinates. Defaults to `"haversine"`.
#' @param columns A character string of the selected variable names. This field
#'   is a placeholder and will be populated once [recipes::prep()] is used.
#' @param crs A named list of the CRS of each geometry column, recorded when
#'   [recipes::prep()] is used. This field is a placeholder and is not meant to
#'   be set by the user.
#' @param keep_original_cols A logical to keep the original geometry columns
#'   after the distances are computed. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @details
#'
#' Only point geometries are supported. Rows with an empty point (such as the
#' ones [step_sfc_point()] creates for missing coordinates) get a distance of
#' `NA`.
#'
#' The new columns are plain numeric vectors, not `units` objects, and depend
#' on what is passed to the step:
#'
#' - With `ref`, every selected column gets one new column for each point in
#'   `ref`, named `{column}_dist_{i}` where `i` is the position of the point in
#'   `ref`. One reference point gives one column and five reference points give
#'   five, each holding the distance from that row to that point. The distances
#'   to all points are computed for every row, so a large `ref` is costly in
#'   both time and memory.
#' - With `ref_col`, every selected column gets a single new column named
#'   `{column}_dist`. The distance is computed row by row, between the point in
#'   the selected column and the point in `ref_col` on the same row.
#'
#' The CRS of the geometry columns is recorded in [recipes::prep()], and
#' [recipes::bake()] errors if the CRS of the new data differs.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `metric`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{metric}{character, the distance metric}
#'   \item{id}{character, id of this step}
#' }
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(ames, package = "modeldata")
#'
#' # Hand-picked points: the Iowa State University Campanile and the Ames Main
#' # Street Historic District
#' fixed_points <- sf::st_sfc(
#'   sf::st_point(c(-93.6460, 42.0254)),
#'   sf::st_point(c(-93.6136, 42.0256)),
#'   crs = 4326
#' )
#'
#' rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
#'   step_sfc_point(lon = Longitude, lat = Latitude) |>
#'   step_spatial_distance(all_geometry_predictors(), ref = fixed_points) |>
#'   prep()
#'
#' # One column per reference point
#' bake(rec, new_data = NULL)
#'
#' tidy(rec, number = 2)
step_spatial_distance <-
  function(
    recipe,
    ...,
    ref = NULL,
    ref_col = NULL,
    metric = c("haversine", "euclidean", "manhattan"),
    role = "predictor",
    trained = FALSE,
    columns = NULL,
    crs = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("spatial_distance")
  ) {
    ref_col <- rlang::enexpr(ref_col)
    has_ref <- !is.null(ref)
    has_ref_col <- !is.null(ref_col)
    if (has_ref_col) {
      if (!rlang::is_symbol(ref_col) && !rlang::is_string(ref_col)) {
        cli::cli_abort("{.arg ref_col} must be a single column name.")
      }
      ref_col <- rlang::as_string(ref_col)
    }

    if (has_ref && has_ref_col) {
      cli::cli_abort(
        c(
          "Both {.arg ref} and {.arg ref_col} were supplied.",
          i = "Supply only one of them."
        )
      )
    }
    if (!has_ref && !has_ref_col) {
      cli::cli_abort(
        c(
          "Neither {.arg ref} nor {.arg ref_col} was supplied.",
          i = "Supply {.arg ref} to measure distances to fixed points, or {.arg ref_col} to measure distances to another geometry column."
        )
      )
    }

    metric <- rlang::arg_match(metric)
    if (has_ref) {
      ref <- check_reference(ref)
    }

    add_step(
      recipe,
      step_spatial_distance_new(
        terms = rlang::enquos(...),
        ref = ref,
        ref_col = if (has_ref_col) ref_col else NULL,
        metric = metric,
        role = role,
        trained = trained,
        columns = columns,
        crs = crs,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_spatial_distance_new <-
  function(
    terms,
    ref,
    ref_col,
    metric,
    role,
    trained,
    columns,
    crs,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "spatial_distance",
      terms = terms,
      ref = ref,
      ref_col = ref_col,
      metric = metric,
      role = role,
      trained = trained,
      columns = columns,
      crs = crs,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_spatial_distance <- function(x, training, info = NULL, ...) {
  if (isTRUE(x$trained)) {
    return(x)
  }

  columns <- recipes::recipes_eval_select(x$terms, training, info)

  ref_col <- x$ref_col
  columns <- setdiff(columns, ref_col)

  crs <- record_crs(training, c(columns, ref_col))
  for (col in columns) {
    check_points(training[[col]], col)
    check_metric_crs(x$metric, crs[[col]], col)
  }

  ref <- NULL
  if (is.null(ref_col)) {
    ref <- list()
    for (col in columns) {
      ref[[col]] <- reference_in_crs(x$ref, crs[[col]], col)
    }
  } else {
    check_points(training[[ref_col]], ref_col)
    check_metric_crs(x$metric, crs[[ref_col]], ref_col)
    for (col in columns) {
      if (!crs_equal(crs[[col]], crs[[ref_col]])) {
        cli::cli_abort(c(
          "Column {.field {col}} and {.field {ref_col}} must have the same CRS.",
          i = "{.field {col}}: {crs_label(crs[[col]])}.",
          i = "{.field {ref_col}}: {crs_label(crs[[ref_col]])}.",
          i = "Use {.fn sf::st_transform} to put them in the same CRS."
        ))
      }
    }
  }

  step_spatial_distance_new(
    terms = x$terms,
    ref = ref,
    ref_col = ref_col,
    metric = x$metric,
    role = x$role,
    trained = TRUE,
    columns = columns,
    crs = crs,
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_spatial_distance <- function(object, new_data, ...) {
  col_names <- c(object$columns, object$ref_col)
  check_new_data(col_names, object, new_data)
  check_crs(new_data, object$crs)
  for (col in col_names) {
    check_points(new_data[[col]], col)
  }

  res <- list()
  for (col in object$columns) {
    xy <- point_coords(new_data[[col]])
    if (is.null(object$ref_col)) {
      dist <- distance_to_ref(
        xy,
        point_coords(object$ref[[col]]),
        object$metric
      )
      new_names <- paste0(col, "_dist_", seq_len(ncol(dist)))
    } else {
      ref_xy <- point_coords(new_data[[object$ref_col]])
      f <- distance_fun(object$metric)
      dist <- matrix(f(xy[, 1], xy[, 2], ref_xy[, 1], ref_xy[, 2]), ncol = 1)
      new_names <- paste0(col, "_dist")
    }
    for (i in seq_along(new_names)) {
      res[[new_names[[i]]]] <- dist[, i]
    }
  }

  if (length(res) > 0) {
    res <- tibble::new_tibble(res, nrow = nrow(new_data))
    recipes::check_name(res, new_data, object, newname = names(res))
    new_data <- vctrs::vec_cbind(new_data, res, .name_repair = "minimal")
  }
  recipes::remove_original_cols(new_data, object, object$columns)
}

#' @export
print.step_spatial_distance <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Spatial distance on "
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
tidy.step_spatial_distance <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$columns
  } else {
    terms <- unname(sel2char(x$terms))
  }
  res <- tibble(
    terms = terms,
    metric = x$metric
  )
  res$id <- x$id
  res
}

#' @rdname required_pkgs.spatialrecipes
#' @export
required_pkgs.step_spatial_distance <- function(x, ...) {
  c("spatialrecipes", "sf")
}
