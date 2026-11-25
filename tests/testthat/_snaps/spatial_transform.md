# a projected CRS lets step_spatial_distance() use euclidean

    Code
      prep(step_spatial_distance(rec, geometry, ref = ref, metric = "euclidean"))
    Condition
      Error in `step_spatial_distance()`:
      Caused by error in `prep()`:
      ! "euclidean" distances need a projected CRS, but geometry uses longitude and latitude.
      i Use `metric = "haversine"`, or use `step_spatial_transform()` to transform the data to a projected CRS.

# crs must be supplied and valid

    Code
      step_spatial_transform(rec, geometry)
    Condition
      Error in `step_spatial_transform()`:
      ! `crs` is absent but must be supplied.

---

    Code
      step_spatial_transform(rec, geometry, crs = NA)
    Condition
      Error in `step_spatial_transform()`:
      ! `crs` must be a single CRS that `sf::st_crs()` understands.
      i For example an EPSG code like `26915` or a string like `"EPSG:26915"`.

---

    Code
      step_spatial_transform(rec, geometry, crs = "not a crs")
    Condition
      Error in `step_spatial_transform()`:
      ! `crs` must be a single CRS that `sf::st_crs()` understands.
      i For example an EPSG code like `26915` or a string like `"EPSG:26915"`.
      Caused by error in `st_crs.character()`:
      ! invalid crs: not a crs

---

    Code
      step_spatial_transform(rec, geometry, crs = c(4326, 3857))
    Condition
      Error in `step_spatial_transform()`:
      ! `crs` must be a single CRS that `sf::st_crs()` understands.
      i For example an EPSG code like `26915` or a string like `"EPSG:26915"`.

---

    Code
      step_spatial_transform(rec, geometry, crs = ames_fixed_points())
    Condition
      Error in `step_spatial_transform()`:
      ! `crs` must be a single CRS that `sf::st_crs()` understands.
      i For example an EPSG code like `26915` or a string like `"EPSG:26915"`.

# columns without a CRS error

    Code
      prep(rec)
    Condition
      Error in `step_spatial_transform()`:
      Caused by error in `prep()`:
      ! Column geometry has no CRS, so it can't be transformed.
      i Use `sf::st_set_crs()` to set the CRS of the data first.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Operations 
      * Spatial transform on: <none>

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
      Training data contained 2 data points and no incomplete rows.
      
      -- Operations 
      * Spatial transform on: <none> | Trained

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
      * Spatial transform on: geometry

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
      Training data contained 2 data points and no incomplete rows.
      
      -- Operations 
      * Spatial transform on: geometry | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = data[, "y"])
    Condition
      Error in `step_spatial_transform()`:
      ! The following required column is missing from `new_data`: geometry.

