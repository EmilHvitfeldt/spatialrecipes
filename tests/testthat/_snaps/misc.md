# record_crs() errors on non-sfc columns

    Code
      record_crs(df, "a")
    Condition
      Error:
      ! Column a must be an <sfc> geometry column.

# record_crs() errors on missing columns

    Code
      record_crs(df, "typo")
    Condition
      Error:
      ! Column typo is not present in the data.

# check_crs() errors on unnamed crs

    Code
      check_crs(df, list(sf::st_crs(4326)))
    Condition
      Error:
      ! `crs` must be a named list.

# check_crs() errors when the CRS differs

    Code
      check_crs(new, crs)
    Condition
      Error:
      ! The CRS of column a in `new_data` does not match the CRS used in `prep()`.
      i Expected: EPSG:4326.
      x Actual: EPSG:4269.

---

    Code
      check_crs(none, crs)
    Condition
      Error:
      ! The CRS of column a in `new_data` does not match the CRS used in `prep()`.
      i Expected: EPSG:4326.
      x Actual: no CRS.

# check_crs() errors when the column is missing or not sfc

    Code
      check_crs(tibble::tibble(b = 1), crs)
    Condition
      Error:
      ! Column a must be an <sfc> geometry column in `new_data`.

---

    Code
      check_crs(tibble::tibble(a = 1), crs)
    Condition
      Error:
      ! Column a must be an <sfc> geometry column in `new_data`.

