test_that("record_crs() records the CRS of each sfc column", {
  df <- tibble::tibble(
    a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326),
    b = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4269)
  )

  expect_identical(
    record_crs(df, c("a", "b")),
    list(a = sf::st_crs(4326), b = sf::st_crs(4269))
  )
})

test_that("record_crs() errors on non-sfc columns", {
  df <- tibble::tibble(a = 1:2)

  expect_snapshot(error = TRUE, record_crs(df, "a"))
})

test_that("record_crs() errors on missing columns", {
  df <- tibble::tibble(a = 1:2)

  expect_snapshot(error = TRUE, record_crs(df, "typo"))
  expect_error(record_crs(df, "typo"), class = "spatialrecipes_crs_column")
})

test_that("check_crs() errors on unnamed crs", {
  df <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))

  expect_snapshot(error = TRUE, check_crs(df, list(sf::st_crs(4326))))
})

test_that("crs_equal() treats equivalent CRS definitions as equal", {
  expect_identical(crs_equal(sf::st_crs(4326), sf::st_crs("EPSG:4326")), TRUE)
  expect_identical(
    crs_equal(sf::st_crs(4326), sf::st_crs(sf::st_crs(4326)$wkt)),
    TRUE
  )
  expect_identical(crs_equal(sf::st_crs(4326), sf::st_crs(4269)), FALSE)
})

test_that("crs_label() handles long inputs and missing names", {
  long <- sf::st_crs(sf::st_crs(4326)$wkt)

  expect_identical(crs_label(long), "WGS 84")
  expect_identical(crs_label(sf::st_crs(NA)), "no CRS")
})

test_that("check_crs() passes when the CRS matches", {
  df <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
  crs <- record_crs(df, "a")

  expect_identical(check_crs(df, crs), df)
})

test_that("check_crs() errors when the CRS differs", {
  train <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
  crs <- record_crs(train, "a")
  new <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4269))
  none <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2))))

  expect_snapshot(error = TRUE, check_crs(new, crs))
  expect_snapshot(error = TRUE, check_crs(none, crs))
  expect_error(check_crs(new, crs), class = "spatialrecipes_crs_mismatch")
})

test_that("check_crs() passes when both CRSs are missing", {
  df <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2))))
  crs <- record_crs(df, "a")

  expect_identical(check_crs(df, crs), df)
})

test_that("check_crs() errors when the column is missing or not sfc", {
  train <- tibble::tibble(a = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
  crs <- record_crs(train, "a")

  expect_snapshot(error = TRUE, check_crs(tibble::tibble(b = 1), crs))
  expect_snapshot(error = TRUE, check_crs(tibble::tibble(a = 1), crs))
  expect_error(
    check_crs(tibble::tibble(a = 1), crs),
    class = "spatialrecipes_crs_column"
  )
})
