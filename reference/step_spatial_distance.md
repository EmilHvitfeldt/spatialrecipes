# Distance to a reference location

`step_spatial_distance()` creates a *specification* of a recipe step
that will compute the distance from each point in one or more geometry
columns to a reference: a fixed set of points (`ref`), or a second
geometry column in the data (`ref_col`).

## Usage

``` r
step_spatial_distance(
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
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which geometry columns (`sfc`
  columns of points) to compute distances from, for example
  [`all_geometry_predictors()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/all_geometry.md).
  See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- ref:

  An `sfc` or `sf` object of points to measure distances to. It is
  evaluated when the step is created and stored in the step. It is
  transformed to the CRS of each selected column when the recipe is
  trained. Exactly one of `ref` and `ref_col` must be given.

- ref_col:

  Bare name of a second geometry column in the data to measure distances
  to, row by row. It must select exactly one `sfc` column with the same
  CRS as the columns in `...`, and is never itself used as a column to
  measure from. Exactly one of `ref` and `ref_col` must be given.

- metric:

  A single string, one of `"haversine"`, `"euclidean"` or `"manhattan"`.
  `"haversine"` measures great-circle distance on a sphere with a radius
  of 6371008.8 meters, requires longitude and latitude coordinates, and
  returns meters. `"euclidean"` and `"manhattan"` measure in the units
  of a projected (or missing) CRS and error on longitude and latitude
  coordinates. Defaults to `"haversine"`.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is used.

- crs:

  A named list of the CRS of each geometry column, recorded when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is used. This field is a placeholder and is not meant to be set by the
  user.

- keep_original_cols:

  A logical to keep the original geometry columns after the distances
  are computed. Defaults to `FALSE`.

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

Only point geometries are supported. Rows with an empty point (such as
the ones
[`step_sfc_point()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/step_sfc_point.md)
creates for missing coordinates) get a distance of `NA`.

The new columns are plain numeric vectors, not `units` objects, and
depend on what is passed to the step:

- With `ref`, every selected column gets one new column for each point
  in `ref`, named `{column}_dist_{i}` where `i` is the position of the
  point in `ref`. One reference point gives one column and five
  reference points give five, each holding the distance from that row to
  that point. The distances to all points are computed for every row.

- With `ref_col`, every selected column gets a single new column named
  `{column}_dist`. The distance is computed row by row, between the
  point in the selected column and the point in `ref_col` on the same
  row.

The CRS of the geometry columns is recorded in
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html),
and
[`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)
errors if the CRS of the new data differs.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `metric`, and
`id`:

- terms:

  character, the selectors or variables selected

- metric:

  character, the distance metric

- id:

  character, id of this step

## Examples

``` r
library(recipes)
data(ames, package = "modeldata")

# Hand-picked points: the Iowa State University Campanile and the Ames Main
# Street Historic District
fixed_points <- sf::st_sfc(
  sf::st_point(c(-93.6460, 42.0254)),
  sf::st_point(c(-93.6136, 42.0256)),
  crs = 4326
)

rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
  step_sfc_point(lon = Longitude, lat = Latitude) |>
  step_spatial_distance(all_geometry_predictors(), ref = fixed_points) |>
  prep()

# One column per reference point
bake(rec, new_data = NULL)
#> # A tibble: 2,930 × 3
#>    Sale_Price geometry_dist_1 geometry_dist_2
#>         <int>           <dbl>           <dbl>
#>  1     215000           3852.           3202.
#>  2     105000           3758.           3090.
#>  3     172000           3744.           3047.
#>  4     244000           3724.           2868.
#>  5     189900           3990.           4448.
#>  6     195500           3977.           4436.
#>  7     213500           4298.           4478.
#>  8     191500           4055.           4248.
#>  9     236500           4118.           4258.
#> 10     189000           3801.           4287.
#> # ℹ 2,920 more rows

tidy(rec, number = 2)
#> # A tibble: 1 × 3
#>   terms    metric    id                    
#>   <chr>    <chr>     <chr>                 
#> 1 geometry haversine spatial_distance_lYqjR
```
