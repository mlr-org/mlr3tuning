#' @title Terminator that stops when a performance level has been reached
#'
#' @aliases mlr_terminators_perf_reached
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning after a performance level has been hit.
#'
#' @section Construction:
#' ```
#' TerminatorPerfReached$new()
#' term("perf_reached")
#' ```
#'
#' @section Parameters:
#' * `level` :: `numeric(1)`\cr
#'   Performance level that needs to be reached, default is 0.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized) this value.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorPerfReached$new()
#' term("perf_reached")
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("level", default = 0, tags = "required")
      ))
      ps$values = list(level = 0)
      super$initialize(param_set = ps)
    },

    is_terminated = function(instance) {
      pv = self$param_set$values
      m = instance$measures[[1L]]
      aggr = instance$archive(unnest = "no")
      if (m$minimize) {
        any(aggr[[m$id]] <= pv$level)
      } else {
        any(aggr[[m$id]] >= pv$level)
      }
    }
  )
)

mlr_terminators$add("perf_reached", TerminatorPerfReached)
