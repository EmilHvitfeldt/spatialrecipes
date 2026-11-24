# Select geometry columns

`all_geometry()` selects all
[`sfc`](https://r-spatial.github.io/sf/reference/sfc.html) columns,
regardless of role. `all_geometry_predictors()` selects all `sfc`
columns that have the `"predictor"` role.

## Usage

``` r
all_geometry()

all_geometry_predictors()
```

## Value

A selector function for use in recipe steps.

## Details

recipes types an `sfc` column as `"other"`, which would also match any
other list-column. These selectors match on the `sfc` class instead.

## See also

[`recipes::has_role()`](https://recipes.tidymodels.org/reference/has_role.html),
[`recipes::has_type()`](https://recipes.tidymodels.org/reference/has_role.html)

## Examples

``` r
library(recipes)
data(ames, package = "modeldata")

rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames) |>
  step_sfc_point(lon = Longitude, lat = Latitude) |>
  prep()

recipes_eval_select(
  rlang::quos(all_geometry_predictors()),
  data = bake(rec, new_data = NULL),
  info = summary(rec)
)
#>   geometry 
#> "geometry" 
```
