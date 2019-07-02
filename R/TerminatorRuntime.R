#' @title Terminator with Runtime Criterion
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a given runtime budget is exceeded.
#' Runtime measurements starts with `update_start()` and ends with `update_end()`.
#' Note that the runtime is checked after each step and therefore it is possible that the final runtime is longer than the specified one.
#'
#' @section Construction:
#' ```
#' t = TerminatorRuntime$new(runtime)
#' ```
#'
#' * `time` :: `numeric(1)`\cr
#'   Maximum allowed runtime, in seconds.
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @export
#' @examples
#' t = TerminatorRuntime$new(3)
TerminatorRuntime = R6Class("TerminatorRuntime",
  inherit = Terminator,
  public = list(

    initialize = function(runtime) {
      assert_number(runtime, lower = 0)
      super$initialize(settings = list(runtime = runtime))
      self$terminated = FALSE
      self$state = list(start = NULL, remaining = runtime)
    },

    update_start = function(pe) {
      self$state$start = proc.time()[[3L]]
      invisible(self)
    },

    update_end = function(pe) {
      self$state$remaining = self$state$remaining - (proc.time()[[3L]] - self$state$start)
      if (self$state$remaining < 0) {
        self$terminated = TRUE
      }
      invisible(self)
    },

    print = function() {
      catf("%s with %.3f seconds remaining.", format(self), self$state$remaining)
    }
  )
)
