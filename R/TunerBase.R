#' @title TunerBase
#'
#' @description
#' TunerBase.
#'
#' @section Usage:
#' ```
#' l = TunerBase(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerBase].
#'
#' @name TunerBase
#' @keywords internal
#' @family TunerBase
NULL

#' @export
TunerBase = R6Class("TunerBase",
  public = list(

    tuner_settings = NULL,
    tune_terminators = NULL,
    fitness_function_class = NULL,
    tune_state = NULL,
  
    initialize = function(id, tuner_settings, tune_terminators, fitness_function_class) {
    
    },

    tune = function(task, learner) {
      stop("tune() not implemented for TunerBase.")
    }
    
  ),
  active = list(),
  private = list()
)