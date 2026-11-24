# selecting more than one column errors

    Code
      prep(step_sfc_point(rec, lon = c(lon, other), lat = lat))
    Condition
      Error in `step_sfc_point()`:
      Caused by error in `prep()`:
      ! only 1 selection is allowed in `lon`, not 2.

# non-numeric columns error

    Code
      prep(step_sfc_point(rec, lon = lon, lat = lat))
    Condition
      Error in `step_sfc_point()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `lon`

# invalid arguments error

    Code
      step_sfc_point(rec, lon = lon, lat = lat, name = 1)
    Condition
      Error in `step_sfc_point()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      prep(step_sfc_point(rec, lon = lon, lat = lat, crs = sf::NA_crs_))
    Condition
      Error in `step_sfc_point()`:
      Caused by error in `prep()`:
      ! `crs` must be a valid coordinate reference system, not `NA`.

# name collision errors

    Code
      prep(rec)
    Condition
      Error in `step_sfc_point()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `y`

# bake errors when columns are missing from new_data

    Code
      bake(rec, new_data = df[, c("y", "lon")])
    Condition
      Error in `bake()`:
      x The following required columns are missing from `new_data`: `lat`.
      i These columns have one of the following roles, which are required at `bake()` time: `predictor`.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * Point geometry from: Longitude Latitude

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 2930 data points and no incomplete rows.
      
      -- Operations 
      * Point geometry from: Longitude Latitude | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = df[, "y"])
    Condition
      Error in `step_sfc_point()`:
      ! The following required columns are missing from `new_data`: lon and lat.

