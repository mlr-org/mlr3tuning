#' @title Terminator with Number of Evaluations Criterion
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning depending on the number of evaluations.
#' An evaluation is defined by one resampling of a parameter value.
#'
#' @section Construction:
#' ```
#' t = TerminatorEvaluations$new(max_evaluations)
#' ```
#'
#' * `max_evaluations` :: `integer(1)`\cr
#'   Number of allowed evaluations.
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvaluations$new(3)
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = Terminator,
  public = list(

    initialize = function(max_evaluations) {
      super$initialize(settings = list(max_evaluations = assert_count(max_evaluations, positive = TRUE, coerce = TRUE)))
      self$terminated = FALSE
      self$state = list(evals = 0L)
    },

    update_start = function(pe) {
      self$state$evals = if (is.null(pe$bmr)) 0L else pe$bmr$data[, data.table::uniqueN(get("hash"))]
      self$terminated = (self$state$evals >= self$settings$max_evaluations)
      invisible(self)
    },

    update_end = function(pe) {
      self$update_start(pe)
    },

    print = function() {
      catf("%s (remaining: %s)", format(self), self$remaining)
    }
  ),

  active = list(
    remaining = function() {
      sprintf("%i evaluations", max(self$settings$max_evaluations - self$state$evals, 0L))
    }
  )
)
