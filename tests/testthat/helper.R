# Reference data built from `modeldata::ames`, see issue #3.

ames_points <- function() {
  sf::st_as_sf(
    modeldata::ames[, c(
      "Sale_Price",
      "Year_Built",
      "Neighborhood",
      "Longitude",
      "Latitude"
    )],
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )
}

# Splits ames into disjoint rows to transform (`data`) and held-out reference
# points (`reference`, an sf tibble with Sale_Price, Year_Built, Neighborhood).
ames_reference_split <- function(n = 100, seed = 1) {
  ames <- modeldata::ames
  withr::with_seed(seed, {
    idx <- sample(nrow(ames), n)
  })
  list(
    data = ames[-idx, ],
    reference = ames_points()[idx, ]
  )
}

# Convex hull of the homes in each neighborhood with at least 3 homes.
ames_reference_polygons <- function() {
  pts <- ames_points()
  groups <- split(sf::st_geometry(pts), pts$Neighborhood)
  groups <- groups[lengths(groups) >= 3]
  hulls <- lapply(groups, \(x) sf::st_convex_hull(sf::st_union(x))[[1]])
  sf::st_sf(
    Neighborhood = names(hulls),
    geometry = sf::st_sfc(hulls, crs = 4326)
  )
}

ames_fixed_points <- function() {
  sf::st_sfc(
    sf::st_point(c(-93.6465, 42.0267)),
    sf::st_point(c(-93.6199, 42.0347)),
    crs = 4326
  )
}

# Points in a projected (or missing) CRS, with known planar distances.
planar_points <- function(xy, crs = 3857) {
  sf::st_sfc(lapply(seq_len(nrow(xy)), \(i) sf::st_point(xy[i, ])), crs = crs)
}

geometry_data <- function(geometry) {
  tibble::tibble(y = seq_along(geometry), geometry = geometry)
}
