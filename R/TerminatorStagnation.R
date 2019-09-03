#' @title Terminator that stops when a performance level has been reached
#'
#' @aliases mlr_terminators_stagnation
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning after the performance stagnates, i.e. does not improve more than
#' `tol` over the last `iters` iterations.
#'
#' TODO: NA handling.
#'
#' @section Construction:
#' ```
#' t = TerminatorStagnation$new(iters, tol = sqrt(.Machine$double.eps))
#' ```
#'
#' * `iters` :: `integer(1)`\cr
#'   Number of iterations to evaluate the performance improvement on.
#'
#' * `tol` :: `numeric(1)`\cr
#'   Tolerance threshold. If the improvement is less than `tol`, tuning is stopped.
#'
#' @family Terminator
#' @export
TerminatorStagnation = R6Class("TerminatorStagnation",
  inherit = Terminator,
  public = list(
    initialize = function(iters, tol = sqrt(.Machine$double.eps)) {
      assert_count(iters, positive = TRUE)
      assert_number(tol, lower = 0)
      super$initialize(settings = list(iters = iters, tol = tol))
    },

    is_terminated = function(inst) {
      iters = self$settings$iters
      if (nrow(aggr) <= iters) {
        return(FALSE)
      }

      aggr = inst$archive(unnest = FALSE)
      m = inst$measures[[1L]]

      perf_before = head(aggr[[m$id]], -iters)
      perf_window = tail(aggr[[m$id]],  iters)

      if (m$minimize) {
        min(perf_window, na.rm = TRUE) > min(perf_before, na.rm = TRUE) - self$settings$tol
      } else {
        max(perf_window, na.rm = TRUE) < max(perf_before, na.rm = TRUE) + self$settings$tol
      }
    }
  )
)

mlr_terminators$add("stagnation", TerminatorStagnation)
