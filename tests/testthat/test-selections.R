test_that("all_geometry() selects sfc columns regardless of role", {
  dat <- modeldata::ames[1:20, c("Sale_Price", "Longitude", "Latitude")]
  dat$other_geometry <- sf::st_sfc(
    lapply(1:20, \(i) sf::st_point(c(i, i))),
    crs = 4326
  )

  res <- recipe(Sale_Price ~ ., data = dat) |>
    update_role(other_geometry, new_role = "id") |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    prep()

  expect_named(
    recipes_eval_select(
      rlang::quos(all_geometry()),
      bake(res, new_data = NULL),
      summary(res)
    ),
    c("other_geometry", "geometry")
  )
})

test_that("all_geometry_predictors() selects only predictor sfc columns", {
  dat <- modeldata::ames[1:20, c("Sale_Price", "Longitude", "Latitude")]
  dat$other_geometry <- sf::st_sfc(
    lapply(1:20, \(i) sf::st_point(c(i, i))),
    crs = 4326
  )

  res <- recipe(Sale_Price ~ ., data = dat) |>
    update_role(other_geometry, new_role = "id") |>
    step_sfc_point(lon = Longitude, lat = Latitude) |>
    prep()

  expect_named(
    recipes_eval_select(
      rlang::quos(all_geometry_predictors()),
      bake(res, new_data = NULL),
      summary(res)
    ),
    "geometry"
  )
})

test_that("all_geometry_predictors() excludes non-geometry predictors", {
  dat <- modeldata::ames[1:20, c("Sale_Price", "Longitude", "Latitude")]

  res <- recipe(Sale_Price ~ ., data = dat) |>
    step_sfc_point(
      lon = Longitude,
      lat = Latitude,
      keep_original_cols = TRUE
    ) |>
    prep()

  expect_named(
    recipes_eval_select(
      rlang::quos(all_geometry_predictors()),
      bake(res, new_data = NULL),
      summary(res)
    ),
    "geometry"
  )
})

test_that("selectors do not match other list-columns", {
  dat <- tibble::tibble(y = 1:3, x = list(1, 2, 3))

  expect_length(
    recipes_eval_select(
      rlang::quos(all_geometry()),
      dat,
      summary(recipe(y ~ x, data = dat))
    ),
    0
  )
})
