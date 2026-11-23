#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @param ... Not used.
#' @return A character vector
#' @name required_pkgs.spatialrecipes
#' @keywords internal
NULL
