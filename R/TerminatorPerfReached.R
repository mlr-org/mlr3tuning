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
#' TerminatorPerfReached$new(level)
#' term("perf_reached")
#' ```
#'
#' * `measure` :: `character(1)`\cr
#'   Name of a measure in the [TuningInstance].
#'   Stored in the parameter set `$param_set`.
#'
#' * `threshold` :: `numeric(1)`\cr
#'   Performance threshold that needs to be reached.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized).
#'   Stored in the parameter set `$param_set`.
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function(measure, threshold) {
      ps = ParamSet$new(list(
        ParamUty$new("measure", tags = "required"),
        ParamDbl$new("threshold", tags = "required")
      ))
      super$initialize(
        param_set = ps,
        param_vals = list(measure = assert_string(measure), threshold = assert_number(threshold))
      )
    },

    is_terminated = function(inst) {
      pv = self$param_set$values
      m = get_by_id(inst$measures, pv$measure)
      if (is.null(m)) {
        stopf("Measure '%s' not being measured by Tuner / TuningInstance!", pv$measure)
      }
      aggr = inst$archive(unnest = FALSE)

      if (m$minimize) {
        any(aggr[[pv$measure]] <= pv$threshold)
      } else {
        any(aggr[[pv$measure]] >= pv$threshold)
      }
    }
  )
)

mlr_terminators$add("perf_reached", TerminatorPerfReached)
