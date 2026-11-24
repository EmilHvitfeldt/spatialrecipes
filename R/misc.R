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

# Reference and distance helpers ----------------------------------------------

check_reference <- function(ref, arg = "ref", call = rlang::caller_env()) {
  if (inherits(ref, "sf")) {
    ref <- sf::st_geometry(ref)
  }
  if (!inherits(ref, "sfc")) {
    cli::cli_abort(
      "{.arg {arg}} must be an {.cls sfc} or {.cls sf} object, not {.obj_type_friendly {ref}}.",
      call = call
    )
  }
  if (length(ref) == 0) {
    cli::cli_abort("{.arg {arg}} must not be empty.", call = call)
  }
  if (any(sf::st_is_empty(ref))) {
    cli::cli_abort(
      "{.arg {arg}} must not contain empty geometries.",
      call = call
    )
  }
  check_points(ref, arg, call = call)
  ref
}

check_points <- function(
  x,
  label,
  call = rlang::caller_env()
) {
  types <- as.character(sf::st_geometry_type(x))
  bad <- types != "POINT" & !sf::st_is_empty(x)
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.field {label}} must only contain point geometries.",
        x = "Found {.val {unique(types[bad])}}.",
        i = "Distances to lines and polygons are not supported yet."
      ),
      call = call
    )
  }
  invisible(x)
}

reference_in_crs <- function(ref, crs, col, call = rlang::caller_env()) {
  ref_crs <- sf::st_crs(ref)
  if (is.na(crs) != is.na(ref_crs)) {
    cli::cli_abort(
      c(
        "{.arg ref} and column {.field {col}} must either both have a CRS or both have none.",
        i = "{.arg ref}: {crs_label(ref_crs)}.",
        i = "{.field {col}}: {crs_label(crs)}."
      ),
      call = call
    )
  }
  if (is.na(crs) || crs_equal(ref_crs, crs)) {
    return(ref)
  }
  sf::st_transform(ref, crs)
}

check_metric_crs <- function(metric, crs, col, call = rlang::caller_env()) {
  longlat <- isTRUE(sf::st_is_longlat(crs))
  if (metric %in% c("euclidean", "manhattan") && longlat) {
    cli::cli_abort(
      c(
        "{.val {metric}} distances need a projected CRS, but {.field {col}} uses longitude and latitude.",
        i = "Use {.code metric = \"haversine\"} or {.code metric = \"geodesic\"}, or {.fn sf::st_transform} the data to a projected CRS."
      ),
      call = call
    )
  }
  if (metric %in% c("haversine", "geodesic") && !longlat) {
    cli::cli_abort(
      c(
        "{.val {metric}} distances need longitude and latitude coordinates, but {.field {col}} has {crs_label(crs)}.",
        i = "Use {.code metric = \"euclidean\"} or {.code metric = \"manhattan\"}, or {.fn sf::st_transform} the data to a geographic CRS."
      ),
      call = call
    )
  }
  invisible(crs)
}

# Two-column matrix of x/y coordinates, `NA` for empty points.
point_coords <- function(x) {
  xy <- vapply(
    unclass(x),
    \(p) if (length(p) == 0) c(NA_real_, NA_real_) else as.numeric(p[1:2]),
    numeric(2)
  )
  matrix(xy, ncol = 2, byrow = TRUE)
}

distance_fun <- function(metric, crs) {
  switch(
    metric,
    euclidean = \(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2),
    manhattan = \(x1, y1, x2, y2) abs(x2 - x1) + abs(y2 - y1),
    haversine = haversine_distance,
    geodesic = {
      a <- as.numeric(crs$SemiMajor)
      b <- as.numeric(crs$SemiMinor)
      if (length(a) != 1 || length(b) != 1 || !is.finite(a) || !is.finite(b)) {
        a <- 6378137
        f <- 1 / 298.257223563
      } else {
        f <- max(0, 1 - b / a)
      }
      \(x1, y1, x2, y2) vincenty_distance(x1, y1, x2, y2, a, f)
    }
  )
}

# Distances in meters on a sphere with the mean Earth radius.
haversine_distance <- function(lon1, lat1, lon2, lat2, r = 6371008.8) {
  p <- pi / 180
  h <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(h)))
}

# Vincenty's inverse formula on an ellipsoid with semi-major axis `a` and
# flattening `f`. Falls back to haversine for nearly antipodal points where the
# iteration does not converge.
vincenty_distance <- function(lon1, lat1, lon2, lat2, a, f) {
  b <- a * (1 - f)
  p <- pi / 180
  u1 <- atan((1 - f) * tan(lat1 * p))
  u2 <- atan((1 - f) * tan(lat2 * p))
  l <- (lon2 - lon1) * p
  sin_u1 <- sin(u1)
  cos_u1 <- cos(u1)
  sin_u2 <- sin(u2)
  cos_u2 <- cos(u2)

  lambda <- l
  delta <- rep(0, length(l))
  for (i in seq_len(200)) {
    sin_lambda <- sin(lambda)
    cos_lambda <- cos(lambda)
    sin_sigma <- sqrt(
      (cos_u2 * sin_lambda)^2 +
        (cos_u1 * sin_u2 - sin_u1 * cos_u2 * cos_lambda)^2
    )
    cos_sigma <- sin_u1 * sin_u2 + cos_u1 * cos_u2 * cos_lambda
    sigma <- atan2(sin_sigma, cos_sigma)
    sin_alpha <- ifelse(
      sin_sigma == 0,
      0,
      cos_u1 * cos_u2 * sin_lambda / sin_sigma
    )
    cos_sq_alpha <- 1 - sin_alpha^2
    cos_2sigma_m <- ifelse(
      abs(cos_sq_alpha) < 1e-15,
      0,
      cos_sigma - 2 * sin_u1 * sin_u2 / cos_sq_alpha
    )
    cc <- f / 16 * cos_sq_alpha * (4 + f * (4 - 3 * cos_sq_alpha))
    lambda_new <- l +
      (1 - cc) *
        f *
        sin_alpha *
        (sigma +
          cc *
            sin_sigma *
            (cos_2sigma_m + cc * cos_sigma * (-1 + 2 * cos_2sigma_m^2)))
    delta <- abs(lambda_new - lambda)
    lambda <- lambda_new
    if (all(delta < 1e-12, na.rm = TRUE)) {
      break
    }
  }

  u_sq <- cos_sq_alpha * (a^2 - b^2) / b^2
  big_a <- 1 + u_sq / 16384 * (4096 + u_sq * (-768 + u_sq * (320 - 175 * u_sq)))
  big_b <- u_sq / 1024 * (256 + u_sq * (-128 + u_sq * (74 - 47 * u_sq)))
  delta_sigma <- big_b *
    sin_sigma *
    (cos_2sigma_m +
      big_b /
        4 *
        (cos_sigma *
          (-1 + 2 * cos_2sigma_m^2) -
          big_b /
            6 *
            cos_2sigma_m *
            (-3 + 4 * sin_sigma^2) *
            (-3 + 4 * cos_2sigma_m^2)))
  s <- b * big_a * (sigma - delta_sigma)

  finite_input <- !is.na(lon1 + lat1 + lon2 + lat2)
  bad <- finite_input & (is.na(s) | is.na(delta) | delta >= 1e-9)
  if (any(bad)) {
    s[bad] <- haversine_distance(lon1[bad], lat1[bad], lon2[bad], lat2[bad])
  }
  s
}

# Distances from every row of `xy` to every row of `ref_xy`, combined across
# reference points. Returns a matrix with one column, or one per reference point
# for `aggregate = "all"`. Done in chunks of rows to limit memory use.
distance_to_ref <- function(xy, ref_xy, metric, aggregate, crs) {
  f <- distance_fun(metric, crs)
  n <- nrow(xy)
  m <- nrow(ref_xy)
  out <- matrix(NA_real_, nrow = n, ncol = if (aggregate == "all") m else 1L)
  if (n == 0) {
    return(out)
  }
  chunk <- max(1L, floor(1e6 / m))
  for (start in seq(1L, n, by = chunk)) {
    idx <- start:min(n, start + chunk - 1L)
    nc <- length(idx)
    d <- matrix(
      f(
        rep(xy[idx, 1], times = m),
        rep(xy[idx, 2], times = m),
        rep(ref_xy[, 1], each = nc),
        rep(ref_xy[, 2], each = nc)
      ),
      nrow = nc
    )
    out[idx, ] <- switch(
      aggregate,
      nearest = apply(d, 1, min),
      mean = rowMeans(d),
      all = d
    )
  }
  out
}
