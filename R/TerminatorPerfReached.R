#' @title Terminator that stops when a peformance level has been reached
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a performance level has been hit.
#'
#' @section Construction:
#' ```
#' t = TerminatorPerfReached$new(level)
#' ```
#' * `level` :: named `numeric(1)`\cr
#'   Level that needs to be reached, named with the ID of the measure we want to check.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized).
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function(level) {
      assert_number(level)
      assert_names(names(level), subset.of = mlr_measures$keys())
      super$initialize(settings = list(level = level))
    },

    is_terminated = function(pe) {
      level = self$settings$level
      mid = names(level)
      aggr = pe$archive()
      m = get_by_id(pe$measures, mid)
      if (is.null(m)) {
        stopf("Measure '%s' not being measured by Tuner / PerfEval!", mid)
      }
      (m$minimize && any(aggr[[mid]] <= self$settings$level)) ||
        (!m$minimize && any(aggr[[mid]] >= self$settings$level))
    }
  )
)
