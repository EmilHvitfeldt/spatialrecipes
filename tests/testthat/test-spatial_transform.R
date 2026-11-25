test_that("columns are transformed to the target CRS", {
  data <- geometry_data(ames_fixed_points())

  res <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("y", "geometry"), ignore.order = TRUE)
  expect_identical(sf::st_crs(res$geometry), sf::st_crs(26915))
  expect_identical(res$geometry, sf::st_transform(data$geometry, 26915))
})

test_that("crs accepts anything sf::st_crs() accepts", {
  data <- geometry_data(ames_fixed_points())

  bake_with <- function(crs) {
    recipe(y ~ ., data = data) |>
      step_spatial_transform(geometry, crs = crs) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_identical(bake_with("EPSG:26915"), bake_with(26915))
  expect_identical(bake_with(sf::st_crs(26915)), bake_with(26915))
})

test_that("bake uses the stored CRS on new data", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  rec <- recipe(Sale_Price ~ ., data = ames[1:100, ]) |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    step_spatial_transform(all_geometry_predictors(), crs = 26915) |>
    prep()
  res <- bake(rec, new_data = ames[101:110, ])

  expect_identical(sf::st_crs(res$geometry), sf::st_crs(26915))
  expect_identical(length(res$geometry), 10L)
})

test_that("columns already in the target CRS are unchanged", {
  data <- geometry_data(planar_points(cbind(1, 2), crs = 3857))
  res <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 3857) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(res$geometry, data$geometry)
})

test_that("empty points stay empty", {
  geom <- sf::st_sfc(sf::st_point(c(-93.6, 42)), sf::st_point(), crs = 4326)
  res <- recipe(y ~ ., data = geometry_data(geom)) |>
    step_spatial_transform(geometry, crs = 26915) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(sf::st_is_empty(res$geometry), c(FALSE, TRUE))
})

test_that("only selected columns are transformed", {
  data <- tibble::tibble(
    y = 1,
    a = planar_points(cbind(1, 2), crs = 4326),
    b = planar_points(cbind(1, 2), crs = 4326)
  )
  res <- recipe(y ~ ., data = data) |>
    step_spatial_transform(a, crs = 3857) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(sf::st_crs(res$a), sf::st_crs(3857))
  expect_identical(sf::st_crs(res$b), sf::st_crs(4326))
})

test_that("a projected CRS lets step_spatial_distance() use euclidean", {
  data <- geometry_data(ames_fixed_points())
  ref <- ames_fixed_points()[1]

  rec <- recipe(y ~ ., data = data)
  expect_snapshot(
    error = TRUE,
    prep(step_spatial_distance(rec, geometry, ref = ref, metric = "euclidean"))
  )

  res <- rec |>
    step_spatial_transform(geometry, crs = 26915) |>
    step_spatial_distance(geometry, ref = ref, metric = "euclidean") |>
    prep() |>
    bake(new_data = NULL)
  expect_identical(res$geometry_dist_1[1], 0)
  expect_gt(res$geometry_dist_1[2], 2000)
})

test_that("crs must be supplied and valid", {
  rec <- recipe(y ~ ., data = geometry_data(ames_fixed_points()))

  expect_snapshot(error = TRUE, step_spatial_transform(rec, geometry))
  expect_snapshot(error = TRUE, step_spatial_transform(rec, geometry, crs = NA))
  expect_snapshot(
    error = TRUE,
    step_spatial_transform(rec, geometry, crs = "not a crs")
  )
  expect_snapshot(
    error = TRUE,
    step_spatial_transform(rec, geometry, crs = c(4326, 3857))
  )
  expect_snapshot(
    error = TRUE,
    step_spatial_transform(rec, geometry, crs = ames_fixed_points())
  )
})

test_that("columns without a CRS error", {
  data <- geometry_data(planar_points(cbind(1, 2), crs = sf::NA_crs_))
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 3857)

  expect_snapshot(error = TRUE, prep(rec))
  expect_error(prep(rec), class = "spatialrecipes_crs_missing")
})

test_that("non-geometry columns error", {
  rec <- recipe(y ~ ., data = tibble::tibble(y = 1, x = 2)) |>
    step_spatial_transform(x, crs = 3857)

  expect_error(prep(rec), class = "spatialrecipes_crs_column")
})

test_that("bake errors when the CRS of new data differs", {
  rec <- recipe(y ~ ., data = geometry_data(planar_points(cbind(1, 2)))) |>
    step_spatial_transform(geometry, crs = 4326) |>
    prep()

  new_data <- geometry_data(planar_points(cbind(1, 2), crs = 32615))
  expect_error(
    bake(rec, new_data = new_data),
    class = "spatialrecipes_crs_mismatch"
  )
})

test_that("prep() on a trained recipe is a no-op", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915) |>
    prep()

  expect_identical(
    bake(prep(rec), new_data = NULL),
    bake(rec, new_data = NULL)
  )
})

test_that("trained recipes survive serialization", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915) |>
    prep()

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(rec, path)

  expect_identical(
    bake(readRDS(path), new_data = NULL),
    bake(rec, new_data = NULL)
  )
})

test_that("tidy method works", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(all_geometry_predictors(), crs = 26915, id = "id")

  expect_identical(
    tidy(rec, number = 1),
    tibble::tibble(
      terms = "all_geometry_predictors()",
      crs = "EPSG:26915",
      id = "id"
    )
  )
  expect_identical(
    tidy(prep(rec), number = 1),
    tibble::tibble(terms = "geometry", crs = "EPSG:26915", id = "id")
  )
})

test_that("required_pkgs() includes sf", {
  rec <- recipe(y ~ ., data = geometry_data(ames_fixed_points())) |>
    step_spatial_transform(geometry, crs = 26915)

  expect_identical(
    required_pkgs(rec$steps[[1]]),
    c("spatialrecipes", "sf")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("empty selection prep/bake is a no-op", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(crs = 26915) |>
    prep()

  expect_identical(bake(rec, new_data = NULL), data[, c("geometry", "y")])
  expect_identical(bake(rec, new_data = data), data[, c("geometry", "y")])
})

test_that("empty selection tidy method works", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(crs = 26915, id = "id")

  expect <- tibble::tibble(
    terms = character(),
    crs = character(),
    id = character()
  )
  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec)
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(crs = 26915)

  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})

test_that("printing", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bake method errors when needed non-standard role columns are missing", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915) |>
    update_role(geometry, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec <- prep(rec, training = data)

  expect_snapshot(error = TRUE, bake(rec, new_data = data[, "y"]))
})

test_that("0 and 1 rows data work in bake method", {
  data <- geometry_data(ames_fixed_points())
  rec <- recipe(y ~ ., data = data) |>
    step_spatial_transform(geometry, crs = 26915) |>
    prep()

  expect_identical(nrow(bake(rec, data[1, ])), 1L)
  expect_identical(nrow(bake(rec, data[0, ])), 0L)
})
