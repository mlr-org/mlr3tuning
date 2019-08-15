#' @title Terminator that stops afthat stops after runtime is depletedd
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a given runtime budget is exceeded.
#'
#' @section Construction:
#' ```
#' t = TerminatorRuntime$new(runtime)
#' ```
#'
#' * `runtime` :: `numeric(1)`\cr
#'   Maximum allowed runtime, in seconds.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorRuntime$new(3)
#' TerminatorRuntime$new(10 * 3600)
TerminatorRuntime = R6Class("TerminatorRuntime",
  inherit = Terminator,
  public = list(
    initialize = function(runtime) {
      assert_number(runtime, lower = 0)
      super$initialize(settings = list(runtime = runtime))
    },

    is_terminated = function(pe) {
      elapsed = as.numeric(Sys.time()) - as.numeric(pe$start_time)
      return(elapsed > self$settings$runtime)
    }
  )
)
