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
#' t = TerminatorEvaluations$new(evals)
#' ```
#'
#' * `evals` :: `integer(1)`\cr
#'   Number of allowed evaluations.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvaluations$new(3)
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = Terminator,
  public = list(

    initialize = function(n_evals) {
      super$initialize(settings = list(n_evals = assert_count(n_evals, positive = TRUE, coerce = TRUE)))
    },

    eval_after = function(pe) {
      self$terminated = pe$n_evals >= self$settings$n_evals
      invisible(self)
    }
  ),

  active = list(
    remaining = function() {
      "???"
      # sprintf("%i evaluations", max(self$settings$evals - self$state$evals, 0L))
    }
  )
)
