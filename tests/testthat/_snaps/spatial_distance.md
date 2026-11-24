# exactly one of ref and ref_col is required

    Code
      step_spatial_distance(rec, geometry)
    Condition
      Error in `step_spatial_distance()`:
      ! Exactly one of `ref` and `ref_col` must be supplied.

---

    Code
      step_spatial_distance(rec, geometry, ref = ref, ref_col = geometry)
    Condition
      Error in `step_spatial_distance()`:
      ! Exactly one of `ref` and `ref_col` must be supplied.

# aggregate can't be used with ref_col

    Code
      step_spatial_distance(rec, geometry, ref_col = geometry, aggregate = "mean")
    Condition
      Error in `step_spatial_distance()`:
      ! `aggregate` can't be used together with `ref_col`.

# invalid ref errors

    Code
      step_spatial_distance(rec, geometry, ref = 1)
    Condition
      Error in `step_spatial_distance()`:
      ! `ref` must be an <sfc> or <sf> object, not a number.

---

    Code
      step_spatial_distance(rec, geometry, ref = point[0])
    Condition
      Error in `step_spatial_distance()`:
      ! `ref` must not be empty.

---

    Code
      step_spatial_distance(rec, geometry, ref = sf::st_sfc(sf::st_point()))
    Condition
      Error in `step_spatial_distance()`:
      ! `ref` must not contain empty geometries.

---

    Code
      step_spatial_distance(rec, geometry, ref = polygon)
    Condition
      Error in `step_spatial_distance()`:
      ! ref must only contain point geometries.
      x Found "POLYGON".
      i Distances to lines and polygons are not supported yet.

# metric must match the type of CRS

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = geographic), geometry, ref = planar_points(
        cbind(0, 0), crs = 4326)))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! "euclidean" distances need a projected CRS, but geometry uses longitude and latitude.
      i Use `metric = "haversine"` or `metric = "geodesic"`, or `sf::st_transform()` the data to a projected CRS.

---

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = projected), geometry, ref = planar_points(
        cbind(0, 0)), metric = "haversine"))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! "haversine" distances need longitude and latitude coordinates, but geometry has EPSG:3857.
      i Use `metric = "euclidean"` or `metric = "manhattan"`, or `sf::st_transform()` the data to a geographic CRS.

---

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = missing_crs), geometry, ref = planar_points(
        cbind(0, 0), crs = sf::NA_crs_), metric = "geodesic"))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! "geodesic" distances need longitude and latitude coordinates, but geometry has no CRS.
      i Use `metric = "euclidean"` or `metric = "manhattan"`, or `sf::st_transform()` the data to a geographic CRS.

# ref and column must agree on having a CRS

    Code
      prep(step_spatial_distance(rec, geometry, ref = planar_points(cbind(0, 0), crs = sf::NA_crs_)))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! `ref` and column geometry must either both have a CRS or both have none.
      i `ref`: no CRS.
      i geometry: EPSG:3857.

# ref_col must have the same CRS as the columns

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = data), from, ref_col = to))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! Column from and to must have the same CRS.
      i Use `sf::st_transform()` to put them in the same CRS.

# non-point and non-geometry columns error

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = geometry_data(polygon)),
      geometry, ref = ref))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! geometry must only contain point geometries.
      x Found "POLYGON".
      i Distances to lines and polygons are not supported yet.

---

    Code
      prep(step_spatial_distance(recipe(y ~ ., data = tibble::tibble(y = 1, x = 2)),
      x, ref = ref))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! Column x must be an <sfc> geometry column.

# name collision errors

    Code
      prep(rec)
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `geometry_dist`

# bake() errors when new data is not points

    Code
      bake(rec, new_data = bad)
    Condition
      Error in `bake()`:
      ! geometry must only contain point geometries.
      x Found "POLYGON".
      i Distances to lines and polygons are not supported yet.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Operations 
      * Spatial distance on: geometry

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Training information 
      Training data contained 1 data points and no incomplete rows.
      
      -- Operations 
      * Spatial distance on: geometry | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = data[, "y"])
    Condition
      Error in `step_spatial_distance()`:
      ! The following required column is missing from `new_data`: geometry.

