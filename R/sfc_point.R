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
#' @param name A single string, the name of the new geometry column. Defaults
#'   to `"geometry"`.
#' @param keep_original_cols A logical to keep the original longitude and
#'   latitude columns after the geometry column is created. Defaults to `FALSE`.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @details
#'
#' Rows where either coordinate is missing get an empty point (`POINT EMPTY`).
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
#'
#' # Reference data for distance, nearest, count and join steps can be derived
#' # from ames itself. Hold out some homes as reference points so they do not
#' # overlap the rows being transformed (otherwise nearest distances are 0).
#' pts <- sf::st_as_sf(
#'   ames[, c("Sale_Price", "Year_Built", "Neighborhood", "Longitude", "Latitude")],
#'   coords = c("Longitude", "Latitude"),
#'   crs = 4326
#' )
#' held_out <- sample(nrow(ames), 100)
#' reference_points <- pts[held_out, ]
#' training <- ames[-held_out, ]
#'
#' # Convex hull of the homes in each neighborhood with at least 3 homes
#' by_hood <- split(sf::st_geometry(pts), pts$Neighborhood)
#' by_hood <- by_hood[lengths(by_hood) >= 3]
#' reference_polygons <- sf::st_sf(
#'   Neighborhood = names(by_hood),
#'   geometry = sf::st_sfc(
#'     lapply(by_hood, \(x) sf::st_convex_hull(sf::st_union(x))[[1]]),
#'     crs = 4326
#'   )
#' )
#'
#' # Hand-picked points: Iowa State University campus and downtown Ames
#' fixed_points <- sf::st_sfc(
#'   sf::st_point(c(-93.6465, 42.0267)),
#'   sf::st_point(c(-93.6199, 42.0347)),
#'   crs = 4326
#' )
step_sfc_point <-
  function(
    recipe,
    lon,
    lat,
    role = "predictor",
    trained = FALSE,
    crs = 4326,
    name = "geometry",
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
    terms = c(lon = lon, lat = lat),
    role = x$role,
    trained = TRUE,
    crs = x$crs,
    name = x$name,
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sfc_point <- function(object, new_data, ...) {
  col_names <- unname(object$terms)
  check_new_data(col_names, object, new_data)

  geometry <- make_points(
    new_data[[object$terms[["lon"]]]],
    new_data[[object$terms[["lat"]]]],
    crs = object$crs
  )

  res <- tibble::new_tibble(
    rlang::set_names(list(geometry), object$name),
    nrow = nrow(new_data)
  )
  recipes::check_name(res, new_data, object, newname = object$name)

  new_data <- vctrs::vec_cbind(new_data, res, .name_repair = "minimal")
  recipes::remove_original_cols(new_data, object, col_names)
}

make_points <- function(lon, lat, crs) {
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  points <- lapply(seq_along(lon), \(i) make_point(lon[i], lat[i]))
  sf::st_sfc(points, crs = crs)
}

make_point <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(sf::st_point())
  }
  sf::st_point(c(lon, lat))
}

#' @export
print.step_sfc_point <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Point geometry from "
    if (is_trained(x)) {
      untr_obj <- NULL
    } else {
      untr_obj <- unname(x$terms)
    }
    print_step(
      unname(x$terms),
      untr_obj,
      x$trained,
      width = width,
      title = title
    )
    invisible(x)
  }

#' @export
tidy.step_sfc_point <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$terms),
      argument = names(x$terms)
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
