test_that("euclidean and manhattan distances are computed in CRS units", {
  data <- geometry_data(planar_points(cbind(c(3, 0, -6), c(4, 0, 8))))
  ref <- planar_points(cbind(0, 0))

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      all_geometry_predictors(),
      ref = ref,
      metric = "euclidean"
    ) |>
    prep() |>
    bake(new_data = NULL)
  expect_named(res, c("y", "geometry_dist_1"))
  expect_identical(res$geometry_dist_1, c(5, 0, 10))

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      all_geometry_predictors(),
      ref = ref,
      metric = "manhattan"
    ) |>
    prep() |>
    bake(new_data = NULL)
  expect_identical(res$geometry_dist_1, c(7, 0, 14))
})

test_that("missing CRS works for planar metrics", {
  data <- geometry_data(planar_points(cbind(3, 4), crs = sf::NA_crs_))
  ref <- planar_points(cbind(0, 0), crs = sf::NA_crs_)

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(geometry, ref = ref, metric = "euclidean") |>
    prep() |>
    bake(new_data = NULL)
  expect_identical(res$geometry_dist_1, 5)
})

test_that("haversine is the default metric and matches a known value", {
  data <- geometry_data(planar_points(cbind(1, 0), crs = 4326))
  ref <- planar_points(cbind(0, 0), crs = 4326)

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(geometry, ref = ref) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(res$geometry_dist_1, 6371008.8 * pi / 180, tolerance = 1e-9)
})

test_that("each reference point gets a column with its distances", {
  split <- ames_reference_split()
  data <- split$data[1:50, c("Sale_Price", "Longitude", "Latitude")]
  ref <- sf::st_geometry(split$reference)[1:7]

  base <- recipe(Sale_Price ~ ., data = data) |>
    step_sfc_point(lon = Longitude, lat = Latitude)
  geom <- bake(prep(base), new_data = NULL)$geometry
  expected <- unclass(sf::st_distance(geom, ref))

  res <- base |>
    step_spatial_distance(all_geometry_predictors(), ref = ref) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("Sale_Price", paste0("geometry_dist_", 1:7)))
  expect_equal(
    unname(as.matrix(res[, -1])),
    expected,
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
})

test_that("a single reference point gives a single column", {
  data <- geometry_data(ames_fixed_points())

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(geometry, ref = ames_fixed_points()[1]) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("y", "geometry_dist_1"))
  expect_identical(res$geometry_dist_1[1], 0)
})

test_that("ref can be an sf object", {
  ref <- ames_reference_split()$reference
  data <- geometry_data(ames_fixed_points())

  bake_with <- function(ref) {
    recipe(y ~ ., data = data) |>
      step_spatial_distance(geometry, ref = ref) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_identical(bake_with(ref), bake_with(sf::st_geometry(ref)))
})

test_that("ref is transformed to the CRS of the column", {
  data <- geometry_data(ames_fixed_points())
  ref <- ames_reference_split()$reference

  bake_with <- function(ref) {
    recipe(y ~ ., data = data) |>
      step_spatial_distance(geometry, ref = ref) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_equal(
    bake_with(sf::st_transform(ref, 3857)),
    bake_with(ref),
    tolerance = 1e-6
  )
})

test_that("multiple geometry columns each get distances", {
  data <- tibble::tibble(
    y = 1:2,
    a = planar_points(cbind(3:4, 4:5)),
    b = planar_points(cbind(0:1, 0:1))
  )
  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      a,
      b,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("y", "a_dist_1", "b_dist_1"), ignore.order = TRUE)
  expect_identical(res$b_dist_1, c(0, sqrt(2)))
})

test_that("ref_col computes row-wise distances", {
  data <- tibble::tibble(
    y = 1:3,
    from = planar_points(cbind(c(0, 1, 2), c(0, 1, 2))),
    to = planar_points(cbind(c(3, 1, 2), c(4, 1, 5)))
  )

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(from, ref_col = to, metric = "euclidean") |>
    prep() |>
    bake(new_data = NULL)
  expect_named(res, c("to", "y", "from_dist"))
  expect_identical(res$from_dist, c(5, 0, 3))

  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      all_geometry_predictors(),
      ref_col = to,
      metric = "euclidean"
    ) |>
    prep() |>
    bake(new_data = NULL)
  expect_named(res, c("to", "y", "from_dist"))
})

test_that("empty points give NA distances", {
  data <- geometry_data(
    sf::st_sfc(sf::st_point(c(3, 4)), sf::st_point(), crs = 3857)
  )
  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(res$geometry_dist_1, c(5, NA))
})

test_that("works after step_sfc_point() on new data", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  rec <- recipe(Sale_Price ~ ., data = ames[1:100, ]) |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    step_spatial_distance(
      all_geometry_predictors(),
      ref = ames_fixed_points()
    ) |>
    prep()
  res <- bake(rec, new_data = ames[101:110, ])

  expect_named(res, c("Sale_Price", "geometry_dist_1", "geometry_dist_2"))
  expect_identical(nrow(res), 10L)
  expect_all_false(is.na(res$geometry_dist_1))
  expect_all_false(is.na(res$geometry_dist_2))
})

test_that("keep_original_cols works", {
  data <- geometry_data(planar_points(cbind(3, 4)))
  res <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean",
      keep_original_cols = TRUE
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("geometry", "y", "geometry_dist_1"))
})

test_that("trained recipes survive serialization", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(geometry, ref = ames_fixed_points()) |>
    prep()

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(rec, path)

  expect_identical(
    bake(readRDS(path), new_data = NULL),
    bake(rec, new_data = NULL)
  )
})

test_that("exactly one of ref and ref_col is required", {
  rec <- recipe(y ~ ., data = geometry_data(planar_points(cbind(1, 2))))
  ref <- planar_points(cbind(0, 0))

  expect_snapshot(error = TRUE, step_spatial_distance(rec, geometry))
  expect_snapshot(
    error = TRUE,
    step_spatial_distance(rec, geometry, ref = ref, ref_col = geometry)
  )
})

test_that("invalid ref errors", {
  rec <- recipe(y ~ ., data = geometry_data(planar_points(cbind(1, 2))))
  point <- planar_points(cbind(0, 0))
  polygon <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
    crs = 3857
  )

  expect_snapshot(error = TRUE, step_spatial_distance(rec, geometry, ref = 1))
  expect_snapshot(
    error = TRUE,
    step_spatial_distance(rec, geometry, ref = point[0])
  )
  expect_snapshot(
    error = TRUE,
    step_spatial_distance(rec, geometry, ref = sf::st_sfc(sf::st_point()))
  )
  expect_snapshot(
    error = TRUE,
    step_spatial_distance(rec, geometry, ref = polygon)
  )
})

test_that("metric must match the type of CRS", {
  geographic <- geometry_data(planar_points(cbind(1, 2), crs = 4326))
  projected <- geometry_data(planar_points(cbind(1, 2), crs = 3857))
  missing_crs <- geometry_data(planar_points(cbind(1, 2), crs = sf::NA_crs_))

  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = geographic),
      geometry,
      ref = planar_points(cbind(0, 0), crs = 4326),
      metric = "euclidean"
    ))
  )
  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = projected),
      geometry,
      ref = planar_points(cbind(0, 0))
    ))
  )
  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = missing_crs),
      geometry,
      ref = planar_points(cbind(0, 0), crs = sf::NA_crs_)
    ))
  )
})

test_that("ref and column must agree on having a CRS", {
  rec <- recipe(y ~ ., data = geometry_data(planar_points(cbind(1, 2))))

  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      rec,
      geometry,
      ref = planar_points(cbind(0, 0), crs = sf::NA_crs_),
      metric = "euclidean"
    ))
  )
})

test_that("ref_col must have the same CRS as the columns", {
  data <- tibble::tibble(
    y = 1,
    from = planar_points(cbind(1, 2), crs = 3857),
    to = planar_points(cbind(1, 2), crs = 32615)
  )

  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = data),
      from,
      ref_col = to,
      metric = "euclidean"
    ))
  )
})

test_that("non-point and non-geometry columns error", {
  polygon <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
    crs = 3857
  )
  ref <- planar_points(cbind(0, 0))

  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = geometry_data(polygon)),
      geometry,
      ref = ref,
      metric = "euclidean"
    ))
  )
  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(
      recipe(y ~ ., data = tibble::tibble(y = 1, x = 2)),
      x,
      ref = ref,
      metric = "euclidean"
    ))
  )
})

test_that("bake errors when the CRS of new data differs", {
  data <- geometry_data(planar_points(cbind(1, 2), crs = 3857))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    prep()

  new_data <- geometry_data(planar_points(cbind(1, 2), crs = 32615))
  expect_error(
    bake(rec, new_data = new_data),
    class = "spatialrecipes_crs_mismatch"
  )
})

test_that("prep() on a trained recipe is a no-op", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    prep()

  expect_identical(
    bake(prep(rec), new_data = NULL),
    bake(rec, new_data = NULL)
  )
})

test_that("name collision errors", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  data$geometry_dist_1 <- 1
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    )

  expect_snapshot(error = TRUE, prep(rec))
})

test_that("tidy method works", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      all_geometry_predictors(),
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean",
      id = "id"
    )

  expect_identical(
    tidy(rec, number = 1),
    tibble::tibble(
      terms = "all_geometry_predictors()",
      metric = "euclidean",
      id = "id"
    )
  )
  expect_identical(
    tidy(prep(rec), number = 1),
    tibble::tibble(
      terms = "geometry",
      metric = "euclidean",
      id = "id"
    )
  )
})

test_that("required_pkgs() includes sf", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(geometry, ref = planar_points(cbind(0, 0)))

  expect_identical(
    required_pkgs(rec$steps[[1]]),
    c("spatialrecipes", "sf")
  )
})

test_that("distance helpers handle edge cases", {
  expect_identical(
    point_coords(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point())),
    matrix(c(1, NA, 2, NA), ncol = 2)
  )
  expect_identical(
    point_coords(sf::st_sfc(sf::st_point(c(1, 2, 3)))),
    matrix(c(1, 2), ncol = 2)
  )

  expect_identical(haversine_distance(10, 20, 10, 20), 0)
  expect_identical(haversine_distance(NA, 0, 1, 0), NA_real_)
})

test_that("bake() errors when new data is not points", {
  data <- tibble::tibble(y = 1:2, geometry = planar_points(cbind(3:4, 4:5)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = sf::st_sfc(sf::st_point(c(0, 0)), crs = sf::st_crs(data$geometry)),
      metric = "euclidean"
    ) |>
    prep()
  bad <- data
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))
  bad$geometry <- sf::st_sfc(poly, poly, crs = sf::st_crs(data$geometry))
  expect_snapshot(bake(rec, new_data = bad), error = TRUE)
})

test_that("distance_to_ref() returns one column per reference point", {
  xy <- cbind(c(1, 5, 9), c(2, 6, 10))
  ref_xy <- cbind(c(0, 4), c(0, 4))

  expect_identical(
    distance_to_ref(xy, ref_xy, "euclidean"),
    cbind(
      sqrt(c(1, 25, 81) + c(4, 36, 100)),
      sqrt(c(9, 1, 25) + c(4, 4, 36))
    )
  )
  expect_identical(
    dim(distance_to_ref(xy[0, , drop = FALSE], ref_xy, "euclidean")),
    c(0L, 2L)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("empty selection prep/bake is a no-op", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(ref = planar_points(cbind(0, 0))) |>
    prep()

  expect_identical(bake(rec, new_data = NULL), data[, c("geometry", "y")])
  expect_identical(bake(rec, new_data = data), data[, c("geometry", "y")])
})

test_that("empty selection tidy method works", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(ref = planar_points(cbind(0, 0)), id = "id")

  expect <- tibble::tibble(
    terms = character(),
    metric = character(),
    id = character()
  )
  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec)
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(ref = planar_points(cbind(0, 0)))

  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})

test_that("printing", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    )

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bake method errors when needed non-standard role columns are missing", {
  data <- geometry_data(planar_points(cbind(1, 2)))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    update_role(geometry, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec <- prep(rec, training = data)

  expect_snapshot(error = TRUE, bake(rec, new_data = data[, "y"]))
})

test_that("0 and 1 rows data work in bake method", {
  data <- geometry_data(planar_points(cbind(c(1, 2), c(3, 4))))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_distance(
      geometry,
      ref = planar_points(cbind(0, 0)),
      metric = "euclidean"
    ) |>
    prep()

  expect_identical(nrow(bake(rec, data[1, ])), 1L)
  expect_identical(nrow(bake(rec, data[0, ])), 0L)
})
