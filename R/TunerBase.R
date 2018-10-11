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
#' * `settings` (`list`):
#'   The settings for the Tuner.
#' * `terminator` (`Terminator`).
#'   All tuning problems optimized with this Tuner object will be terminated by this terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerBase].
#'
#' @name TunerBase
#' @keywords internal
#' @family Tuner
NULL

#' @export
TunerBase = R6Class("TunerBase",
  public = list(

    id = NULL,
    settings = NULL,
    terminators = NULL,
    fitness_function_class = NULL,
    state = NULL,
    fitness_function = NULL
  
    initialize = function(id, settings, terminator, fitness_function_class = NULL) {
      self$id = assert_character(id)
      self$settings = assert_list(settings)
      self$terminator = assert_r6(terminator, "TerminatorBase")
      self$fitness_function_class = assert_class(fitness_function_class, "R6ClassGenerator") %??% FitnessFunction
    },

    tune = function(task, learner, param_set) {
      stop("tune() not implemented for TunerBase.")
    },

    tune_step = function() {
      stop("tune_step() not implemented for TunerBase.")
    },

    tune_result = function() {
      self$fitness_function$get_best()
    }
    
  ),
  active = list(),
  private = list()
)