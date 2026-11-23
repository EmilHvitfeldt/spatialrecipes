test_that("step_sfc_point() creates a point geometry column", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    prep()
  res <- bake(rec, new_data = NULL)

  expect_named(res, c("Sale_Price", "geometry"))
  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "sf"))
  expect_s3_class(res$geometry, "sfc_POINT")
  expect_equal(sf::st_crs(res$geometry), sf::st_crs(4326))

  expected <- sf::st_geometry(
    sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
  )
  expect_equal(res$geometry, expected)
})

test_that("step_sfc_point() works on new data", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  rec <- recipe(Sale_Price ~ ., data = ames[1:100, ]) |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    prep()
  res <- bake(rec, new_data = ames[101:110, ])

  expect_identical(nrow(res), 10L)
  expect_equal(
    sf::st_coordinates(res$geometry)[, "X"],
    ames$Longitude[101:110]
  )
})

test_that("keep_original_cols works", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  res <- recipe(Sale_Price ~ ., data = ames) |>
    step_sfc_point(
      lon = Longitude,
      lat = Latitude,
      keep_original_cols = TRUE
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("Longitude", "Latitude", "Sale_Price", "geometry"))
})

test_that("crs and name arguments work", {
  ames <- modeldata::ames[, c("Sale_Price", "Longitude", "Latitude")]

  res <- recipe(Sale_Price ~ ., data = ames) |>
    step_sfc_point(lon = Longitude, lat = Latitude, crs = 4269, name = "loc") |>
    prep() |>
    bake(new_data = NULL)

  expect_named(res, c("Sale_Price", "loc"))
  expect_equal(sf::st_crs(res$loc), sf::st_crs(4269))
})

test_that("step_sfc_point() works on sf input and returns a tibble", {
  df <- tibble::tibble(
    id = 1:3,
    lon = c(-122.4, -118.2, -73.9),
    lat = c(37.8, 34.1, 40.7)
  )
  df_sf <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  rec <- recipe(id ~ ., data = df_sf) |>
    step_sfc_point(lon = lon, lat = lat, name = "point") |>
    prep()
  res <- bake(rec, new_data = NULL)

  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "sf"))
  expect_s3_class(res$point, "sfc_POINT")
})

test_that("missing coordinates give empty points", {
  df <- tibble::tibble(
    y = 1:3,
    lon = c(1, NA, 3),
    lat = c(4, 5, NA)
  )

  res <- recipe(y ~ ., data = df) |>
    step_sfc_point(lon = lon, lat = lat) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(sf::st_is_empty(res$geometry), c(FALSE, TRUE, TRUE))
})

test_that("selecting more than one column errors", {
  df <- tibble::tibble(y = 1:2, lon = 1:2, lat = 3:4, other = 5:6)
  rec <- recipe(y ~ ., data = df)

  expect_snapshot(
    error = TRUE,
    prep(step_sfc_point(rec, lon = c(lon, other), lat = lat))
  )
})

test_that("non-numeric columns error", {
  df <- tibble::tibble(y = 1:2, lon = c("a", "b"), lat = 3:4)
  rec <- recipe(y ~ ., data = df)

  expect_snapshot(
    error = TRUE,
    prep(step_sfc_point(rec, lon = lon, lat = lat))
  )
})

test_that("invalid arguments error", {
  df <- tibble::tibble(y = 1:2, lon = 1:2, lat = 3:4)
  rec <- recipe(y ~ ., data = df)

  expect_snapshot(
    error = TRUE,
    step_sfc_point(rec, lon = lon, lat = lat, name = 1)
  )
  expect_snapshot(
    error = TRUE,
    prep(step_sfc_point(rec, lon = lon, lat = lat, crs = sf::NA_crs_))
  )
})

test_that("name collision errors", {
  df <- tibble::tibble(y = 1:2, lon = 1:2, lat = 3:4)
  rec <- recipe(y ~ ., data = df) |>
    step_sfc_point(lon = lon, lat = lat, name = "y")

  expect_snapshot(error = TRUE, prep(rec))
})

test_that("bake errors when columns are missing from new_data", {
  df <- tibble::tibble(y = 1:2, lon = 1:2, lat = 3:4)
  rec <- recipe(y ~ ., data = df) |>
    step_sfc_point(lon = lon, lat = lat) |>
    prep()

  expect_snapshot(error = TRUE, bake(rec, new_data = df[, c("y", "lon")]))
})

test_that("tidy method works", {
  rec <- recipe(Sale_Price ~ Longitude + Latitude, data = modeldata::ames) |>
    step_sfc_point(lon = Longitude, lat = Latitude, id = "id")

  expected <- tibble::tibble(
    terms = c("Longitude", "Latitude"),
    argument = c("lon", "lat"),
    id = "id"
  )

  expect_identical(tidy(rec, number = 1), expected)
  expect_identical(tidy(prep(rec), number = 1), expected)
})

test_that("required_pkgs() includes sf", {
  rec <- recipe(~., data = tibble::tibble(lon = 1, lat = 2)) |>
    step_sfc_point(lon = lon, lat = lat)

  expect_identical(
    required_pkgs(rec$steps[[1]]),
    c("spatialrecipes", "sf")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("printing", {
  rec <- recipe(Sale_Price ~ Longitude + Latitude, data = modeldata::ames) |>
    step_sfc_point(lon = Longitude, lat = Latitude)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
