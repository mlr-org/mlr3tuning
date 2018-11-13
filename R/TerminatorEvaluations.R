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
      super$initialize(id = id, settings = list(max_evaluations = assert_int(max_evaluations, lower = 1L, coerce = TRUE)))
      self$terminated = FALSE
      self$state = list(evals = 0L)
    },

    update_start = function(ff) {
      self$state$evals = nrow(ff$experiments)
      self$terminated = (self$state$evals >= self$settings$max_evaluations)
      invisible(self)
    },

    update_end = function(ff) {
      self$state$evals = nrow(ff$experiments)
      self$terminated = (self$state$evals >= self$settings$max_evaluations)
      invisible(self)
    }
  ),

  active = list(
    message = function() {
      sprintf("Iteration %i/%i (%s)", self$state$evals, self$settings$max_evaluations,
        if (self$terminated) "exhausted" else "not exhausted")
    }
  )
)
