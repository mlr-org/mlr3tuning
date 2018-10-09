#' @title FitnessFunction
#'
#' @description
#' FitnessFunction.
#'
#' @section Usage:
#' ```
#' l = FitnessFunction(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the FitnessFunction.
#'
#' @section Details:
#' `$new()` creates a new object of class [FitnessFunction].
#'
#' @name FitnessFunction
#' @keywords internal
#' @family FitnessFunction
NULL

#' @export
FitnessFunction = R6Class("FitnessFunction",
  public = list(
  
    initialize = function(id) {
    
    }
    
  ),
  active = list(),
  private = list()
)