#' @title Terminator that stops when tuning does not improve
#'
#' @aliases mlr_terminators_stagnation
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning after the performance stagnates, i.e. does not improve more than
#' `threshold` over the last `iters` iterations.
#'
#' @section Construction:
#' ```
#' t = TerminatorStagnation$new(iters, tol = 0)
#' ```
#'
#' * `iters` :: `integer(1)`\cr
#'   Number of iterations to evaluate the performance improvement on.
#'   Stored in the parameter set `$param_set`.
#'
#' * `threshold` :: `numeric(1)`\cr
#'   If the improvement is less than `threshold`, tuning is stopped.
#'   Default is `0`.
#'   Stored in the parameter set `$param_set`.
#'
#' @family Terminator
#' @export
TerminatorStagnation = R6Class("TerminatorStagnation",
  inherit = Terminator,
  public = list(
    initialize = function(iters, threshold = 0) {
      ps = ParamSet$new(list(
        ParamInt$new("iters", lower = 1L, tags = "required"),
        ParamDbl$new("threshold", lower = 0, default = 0, tags = "required")
      ))

      super$initialize(
        param_set = ps,
        param_vals = list(
          iters = assert_count(iters, positive = TRUE, coerce = TRUE),
          threshold = assert_number(threshold, lower = 0)
        )
      )
    },

    is_terminated = function(inst) {
      pv = self$param_set$values
      iters = pv$iters

      aggr = inst$archive(unnest = FALSE)
      if (nrow(aggr) <= iters) {
        return(FALSE)
      }

      m = inst$measures[[1L]]

      perf_before = head(aggr[[m$id]], -iters)
      perf_window = tail(aggr[[m$id]],  iters)

      if (m$minimize) {
        min(perf_window) > min(perf_before) - pv$threshold
      } else {
        max(perf_window) < max(perf_before) + pv$threshold
      }
    }
  )
)

mlr_terminators$add("stagnation", TerminatorStagnation)
