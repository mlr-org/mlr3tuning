#' @title TunerRandomSearch
#'
#' @description
#' TunerRandomSearch
#'
#' @section Usage:
#' ```
#' l = TunerRandomSearch(id)
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
#' `$new()` creates a new object of class [TunerRandomSearch].
#'
#' @name TunerRandomSearch
#' @keywords internal
#' @family Tuner
NULL

#' @export
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerBase,
  public = list(
  
    initialize = function(id, terminators, fitness_function_class = NULL) {
      super$initialize(id = id, tuner_settings = list(), terminators = terminators, fitness_function_class = fitness_function_class)

    },

    tune = function(task, learner) {
      self$fitness_function = self$fitness_function_class$new(
        task = task, learner = learner, resampling = self$resampling, measures = self$measures, param_set = self$param_set, terminator = self$terminator)
      if ("TerminatorEvaluations" %in% class(self$terminator)) {
        n = self$terminator$settings$max_evaluations
      }
    },
    
  ),
  active = list(),
  private = list()
)