# spatialrecipes (development version)

* Initial CRAN submission.

* `all_geometry()` and `all_geometry_predictors()` are new selectors that match `sfc` columns, which recipes otherwise types as `"other"`. (#2)

* `step_sfc_point()` is a new step that combines a longitude column and a latitude column into a point geometry (`sfc`) column. (#1)

* `step_spatial_distance()` is a new step that computes the distance from point geometry columns to a set of reference points or to a second geometry column, using Euclidean, Manhattan or haversine distances. (#6)
