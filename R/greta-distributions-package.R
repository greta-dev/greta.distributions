#' @title Extends Distributions Available in the `greta` package
#' @name greta.distributions
#' 
#' @description describe your package here, you can re-use the text from DESCRIPTION
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tensorflow tf
#' @importFrom greta .internals
#' @importFrom R6 R6Class
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

globalVariables(
  c(
    "as_2d_array",
    "as_data",
    "calculate",
    "initials",
    "prep_initials",
    "variable"
  )
)