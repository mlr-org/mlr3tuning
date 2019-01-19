#' @title TerminatorEvaluations Class
#'
#' @description
#' Class to terminate the tuning depending on the number of evaluations. An evaluation
#' is defined by one resampling of a parameter value.
#'
#' @section Usage:
#' ```
#' # Constructor
#' t = TerminatorEvaluations$new(max_evaluations)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `max_evaluations` (`integer(1)`):
#'   Maximum number of function evaluations.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorEvaluations].
#' 
#' The interface is described in [Terminator].
#'
#' @name TerminatorEvaluations
#' @family Terminator
#' @examples
#' t = TerminatorEvaluations$new(3)
NULL

#' @export
#' @include Terminator.R
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = Terminator,
  public = list(

    initialize = function(max_evaluations) {
      super$initialize(settings = list(max_evaluations = assert_count(max_evaluations, positive = TRUE, coerce = TRUE)))
      self$terminated = FALSE
      self$state = list(evals = 0L)
    },

    update_start = function(ff) {
      self$state$evals = if (is.null(ff$bmr)) 0L else nrow(ff$bmr$aggregated)
      self$terminated = (self$state$evals >= self$settings$max_evaluations)
      invisible(self)
    },

    update_end = function(ff) {
      self$update_start(ff)
    },

    format = function() sprintf("TerminatorEvaluations with %i remaining evaluations", max(self$settings$max_evaluations - self$state$evals, 0L))
  )
)
