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
#' * `measure` :: `character(1)`\cr
#'   Name of a measure in the [TuningInstance], default is "" - which is stupid.
#'
#' * `level` :: `numeric(1)`\cr
#'   Performance level that needs to be reached, default is 0 - which is stupid.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized).
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("measure", default = "", tags = "required"),
        ParamDbl$new("level", default = 0, tags = "required")
      ))
      ps$values = list(measure = "", level = 0)

      super$initialize(param_set = ps)
    },

    is_terminated = function(inst) {
      pv = self$param_set$values
      m = get_by_id(inst$measures, pv$measure)
      if (is.null(m)) {
        stopf("Measure '%s' not being measured by Tuner / TuningInstance!", pv$measure)
      }
      aggr = inst$archive(unnest = FALSE)

      if (m$minimize) {
        any(aggr[[pv$measure]] <= pv$level)
      } else {
        any(aggr[[pv$measure]] >= pv$level)
      }
    }
  )
)

mlr_terminators$add("perf_reached", TerminatorPerfReached)
