#' @title Terminator that stops after a number of evals
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
#' t = TerminatorEvals$new(evals)
#' ```
#'
#' * `evals` :: `integer(1)`\cr
#'   Number of allowed evaluations.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvals$new(3)
TerminatorEvals = R6Class("TerminatorEvals",
  inherit = Terminator,
  public = list(

    initialize = function(n_evals) {
      super$initialize(settings = list(n_evals = assert_count(n_evals, positive = TRUE, coerce = TRUE)))
    },

    eval_after = function(pe) {
      self$is_terminated = pe$n_evals >= self$settings$n_evals
      invisible(self)
    }
  )
)
