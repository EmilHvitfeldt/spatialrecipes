# Changelog

## spatialrecipes (development version)

- Initial CRAN submission.

- [`all_geometry()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/all_geometry.md)
  and
  [`all_geometry_predictors()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/all_geometry.md)
  are new selectors that match `sfc` columns, which recipes otherwise
  types as `"other"`.
  ([\#2](https://github.com/EmilHvitfeldt/spatialrecipes/issues/2))

- [`step_sfc_point()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/step_sfc_point.md)
  is a new step that combines a longitude column and a latitude column
  into a point geometry (`sfc`) column.
  ([\#1](https://github.com/EmilHvitfeldt/spatialrecipes/issues/1))

- [`step_spatial_distance()`](https://emilhvitfeldt.github.io/spatialrecipes/reference/step_spatial_distance.md)
  is a new step that computes the distance from point geometry columns
  to a set of reference points or to a second geometry column, using
  Euclidean, Manhattan or haversine distances.
  ([\#6](https://github.com/EmilHvitfeldt/spatialrecipes/issues/6))
