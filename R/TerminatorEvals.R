#' @title Terminator that stops after a number of evaluations
#'
#' @aliases mlr_terminators_evals
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning depending on the number of evaluations.
#' An evaluation is defined by one resampling of a parameter value.
#'
#' @section Construction:
#' ```
#' TerminatorEvals$new(n_evals = 1L)
#' term("evals")
#' ```
#'
#' * `n_evals` :: `integer(1)`\cr
#'   Number of allowed evaluations.
#'   Stored in the parameter set `$param_set`.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvals$new(3)
#' term("evals", 5)
TerminatorEvals = R6Class("TerminatorEvals",
  inherit = Terminator,
  public = list(

    initialize = function(n_evals = 1L) {
      assert_count(n_evals, positive = TRUE)
      ps = ParamSet$new(list(ParamInt$new("n_evals", lower = 1L, tags = "required")))
      super$initialize(param_set = ps, param_vals = list(n_evals = n_evals))
    },

    is_terminated = function(inst) {
      inst$n_evals >= self$param_set$values$n_evals
    }
  )
)

mlr_terminators$add("evals", TerminatorEvals)
