#' @title Terminator with Time Criterion
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a fixed date / time (as reported by [Sys.time()]).
#'
#' @section Construction:
#' ```
#' t = TerminatorTime$new(time)
#' ```
#'
#' * `time` :: `POSIXct(1)`\cr
#'   Terminator stops after this point in time.
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
#' time = Sys.time() + (60 * 60) # now + 1 hour
#' t = TerminatorTime$new(time)
#' print(t)
#'
#' time = as.POSIXct("2030-01-01 00:00:00")
#' t = TerminatorTime$new(time)
#' print(t)
TerminatorTime = R6Class("TerminatorTime",
  inherit = Terminator,
  public = list(
    initialize = function(time) {
      assert_class(time, "POSIXct")
      super$initialize(settings = list(time = time))
      self$terminated = FALSE
    },

    update_start = function(pe) {
      if (Sys.time() > self$settings$time) {
        self$terminated = TRUE
      }
      invisible(self)
    },

    update_end = function(pe) {
      if (Sys.time() > self$settings$time) {
        self$terminated = TRUE
      }
      invisible(self)
    },

    print = function() {
      dt = self$settings$time - Sys.time()
      catf("%s with %.3f %s remaining.", format(self), dt, units(dt))
    }
  )
)
