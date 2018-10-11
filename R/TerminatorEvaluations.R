#' @title TerminatorEvaluations
#'
#' @description
#' TerminatorEvaluations.
#'
#' @section Usage:
#' ```
#' l = Terminator(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The name of the Terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorEvaluations].
#'
#' @name TerminatorEvaluations
#' @keywords internal
#' @family Terminator
NULL

#' @export
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = TerminatorBase,
  public = list(

    initialize = function(id, max_evaluations) {
      super$initialize(id = id, settings = list(max_evaluations = assert_int(max_evaluations, lower = 1)))
    },

    update_start = function(fitness_function) {
      if (is.null(self$state)) {
        self$terminated = FALSE
        self$state = list(evals = 0)
      }
      evals = nrow(fitness_functions$experiment_store)
      self$terminated = (evals >= self$settings$max_evaluations)
      self$state = list(evals = evals)
      invisible(self$terminated)
    },

    update_end = function(fitness_function) {
      evals = nrow(fitness_functions$experiment_store)
      self$terminated = (evals >= self$settings$max_evaluations)
      self$state = list(evals = evals)
      invisible(self$terminated)
    }
  ),
  active = list(
    message = function() {
      if (self$terminated) {
        sprintf("Budget of %i evaluations exhausted with %i evaluations.", self$settings$max_evaluations, self$state$evals)
      } else {
        sprintf("Budget of %i evaluations not exhausted with %i evaluations.", self$settings$max_evaluations, self$state$evals)
      }
    }
  ),
  private = list()
)