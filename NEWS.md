# spatialrecipes (development version)

* Initial CRAN submission.

* `all_geometry()` and `all_geometry_predictors()` are new selectors that match `sfc` columns, which recipes otherwise types as `"other"`. Loading spatialrecipes now types `sfc` columns as `"sfc"`, so `has_type("other")` no longer matches them. (#2)

* `step_sfc_point()` is a new step that combines a longitude column and a latitude column into a point geometry (`sfc`) column. (#1)
