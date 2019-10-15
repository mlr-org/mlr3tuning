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
#' TerminatorEvals$new()
#' term("evals")
#' ```
#'
#' @section Parameters:
#' * `n_evals` :: `integer(1)`\cr
#'   Number of allowed evaluations, default is 100L
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvals$new()
#' term("evals", n_evals = 5)
TerminatorEvals = R6Class("TerminatorEvals",
  inherit = Terminator,
  public = list(

    initialize = function() {
      ps = ParamSet$new(list(ParamInt$new("n_evals", lower = 1L, default = 100L, tags = "required")))
      ps$values = list(n_evals = 100L)

      super$initialize(param_set = ps)
    },

    is_terminated = function(instance) {
      instance$n_evals >= self$param_set$values$n_evals
    }
  )
)

mlr_terminators$add("evals", TerminatorEvals)
