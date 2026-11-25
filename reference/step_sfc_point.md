# Create point geometries from longitude and latitude

`step_sfc_point()` creates a *specification* of a recipe step that will
combine a longitude column and a latitude column into a single point
geometry column (an `sfc` column, see
[`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html)).

## Usage

``` r
step_sfc_point(
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
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- lon, lat:

  Bare names of the columns holding the longitude (x) and latitude (y)
  coordinates. Each must select exactly one numeric column. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- crs:

  The coordinate reference system of the coordinates, in any form
  accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).
  Defaults to `4326`, WGS 84 longitude and latitude in degrees.

- name:

  A single string, the name of the new geometry column. Defaults to
  `"geometry"`.

- keep_original_cols:

  A logical to keep the original longitude and latitude columns after
  the geometry column is created. Defaults to `FALSE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any).

## Details

Rows where either coordinate is missing get an empty point
(`POINT EMPTY`).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `argument`, and
`id`:

- terms:

  character, the selectors or variables selected

- argument:

  character, either `"lon"` or `"lat"`

- id:

  character, id of this step

## Examples

``` r
library(recipes)
data(ames, package = "modeldata")

rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
  step_sfc_point(lon = Longitude, lat = Latitude)

rec <- prep(rec)

bake(rec, new_data = NULL)
#> # A tibble: 2,930 × 2
#>    Sale_Price             geometry
#>         <int>          <POINT [°]>
#>  1     215000 (-93.61975 42.05403)
#>  2     105000 (-93.61976 42.05301)
#>  3     172000 (-93.61939 42.05266)
#>  4     244000 (-93.61732 42.05125)
#>  5     189900  (-93.63893 42.0609)
#>  6     195500 (-93.63893 42.06078)
#>  7     213500 (-93.63379 42.06298)
#>  8     191500 (-93.63383 42.06073)
#>  9     236500 (-93.63285 42.06112)
#> 10     189000 (-93.63907 42.05919)
#> # ℹ 2,920 more rows

tidy(rec, number = 1)
#> # A tibble: 2 × 3
#>   terms     argument id             
#>   <chr>     <chr>    <chr>          
#> 1 Longitude lon      sfc_point_UUEdL
#> 2 Latitude  lat      sfc_point_UUEdL

# Reference data for distance, nearest, count and join steps can be derived
# from ames itself. Hold out some homes as reference points so they do not
# overlap the rows being transformed (otherwise nearest distances are 0).
pts <- sf::st_as_sf(
  ames[, c("Sale_Price", "Year_Built", "Neighborhood", "Longitude", "Latitude")],
  coords = c("Longitude", "Latitude"),
  crs = 4326
)
held_out <- sample(nrow(ames), 100)
reference_points <- pts[held_out, ]
training <- ames[-held_out, ]

# Convex hull of the homes in each neighborhood with at least 3 homes
by_hood <- split(sf::st_geometry(pts), pts$Neighborhood)
by_hood <- by_hood[lengths(by_hood) >= 3]
reference_polygons <- sf::st_sf(
  Neighborhood = names(by_hood),
  geometry = sf::st_sfc(
    lapply(by_hood, \(x) sf::st_convex_hull(sf::st_union(x))[[1]]),
    crs = 4326
  )
)

# Hand-picked points: the Iowa State University Campanile and the Ames Main
# Street Historic District
fixed_points <- sf::st_sfc(
  sf::st_point(c(-93.6460, 42.0254)),
  sf::st_point(c(-93.6136, 42.0256)),
  crs = 4326
)
```
