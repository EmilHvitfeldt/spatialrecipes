# S3 methods for tracking which additional packages are needed for steps.

Recipe-adjacent packages always list themselves as a required package so
that the steps can function properly within parallel processing schemes.

## Usage

``` r
# S3 method for class 'step_sfc_point'
required_pkgs(x, ...)

# S3 method for class 'step_spatial_distance'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe step

- ...:

  Not used.

## Value

A character vector
