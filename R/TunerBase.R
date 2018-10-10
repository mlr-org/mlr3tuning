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
  
    initialize = function(id, settings, terminators, fitness_function_class = NULL) {
      self$id = assert_character(id)
      self$settings = assert_list(settings)
      self$terminators = assert_list(terminators, types = "TerminatorBase")
      self$fitness_function_class = assert_class(fitness_function_class, "FitnessFunction") %??% FitnessFunction
    },

    tune = function(task, learner) {
      stop("tune() not implemented for TunerBase.")
    },
    
  ),
  active = list(),
  private = list()
)