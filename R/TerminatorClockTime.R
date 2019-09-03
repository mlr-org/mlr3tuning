#' @title Terminator that stops accoring to clock
#'
#' @aliases mlr_terminators_clock_time
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning either after the complete process took a number of seconds on the clock
#' or a fixed time point has been reached (as reported by [Sys.time()]).
#'
#' @section Construction:
#' ```
#' t = TerminatorClockTime$new(secs = NULL, time = NULL)
#' ```
#'
#' * `secs` :: `numeric(1)`\cr
#'   Maximum allowed time, in seconds.
#'   Mutually exclusive with argument `stop_time`.
#'   Stored in `$settings`.
#' * `stop_time` :: `POSIXct(1)`\cr
#'   Terminator stops after this point in time.
#'   Mutually exclusive with argument `secs`.
#'   Stored in `$settings`.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorClockTime$new(secs = 3600)
#'
#' stop_time = as.POSIXct("2030-01-01 00:00:00")
#' term("clock_time", stop_time = stop_time)
TerminatorClockTime = R6Class("TerminatorClockTime",
  inherit = Terminator,
  public = list(
    initialize = function(secs = NULL, stop_time = NULL) {
      assert_number(secs, lower = 0, null.ok = TRUE)
      assert_posixct(stop_time, null.ok = TRUE)
      if (!xor(is.null(secs), is.null(stop_time)))
        stopf("Exactly one argument of 'secs' and 'stop_time' has to be set!")
      super$initialize(settings = list(secs = secs, stop_time = stop_time))
    },

    is_terminated = function(inst) {
      if (!is.null(self$settings$secs)) {
        d = difftime(Sys.time(), inst$start_time, units = "secs")
        return(d >= self$settings$secs)
      } else {
        return(Sys.time() >= self$settings$stop_time)
      }
    }
  )
)

mlr_terminators$add("clock_time", TerminatorClockTime)
