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
#' TerminatorClockTime$new(secs = NULL, stop_time = NULL)
#' term("clock_time")
#' ```
#' * `secs` :: `numeric(1)`\cr
#'   Maximum allowed time, in seconds.
#'   Mutually exclusive with argument `stop_time`.
#'   Stored in the parameter set `$param_set`.
#'
#' * `stop_time` :: `POSIXct(1)`\cr
#'   Terminator stops after this point in time.
#'   Mutually exclusive with argument `secs`.
#'   Stored in the parameter set `$param_set`.
#'
#' @family Terminator
#' @export
#' @examples
#' term("clock_time", secs = 1800)
#'
#' stop_time = as.POSIXct("2030-01-01 00:00:00")
#' term("clock_time", stop_time = stop_time)
TerminatorClockTime = R6Class("TerminatorClockTime",
  inherit = Terminator,
  public = list(
    initialize = function(secs = NULL, stop_time = NULL) {
      assert_number(secs, lower = 0, null.ok = TRUE)
      assert_posixct(stop_time, len = 1L, any.missing = FALSE, null.ok = TRUE)

      super$initialize(
        param_set = ParamSet$new(list(
            ParamDbl$new("secs", lower = 0),
            ParamUty$new("stop_time"))
        ),
        param_vals = discard(list(secs = secs, stop_time = stop_time), is.null)
      )
    },

    is_terminated = function(inst) {
      pv = self$param_set$values
      if (!xor(is.null(pv$secs), is.null(pv$stop_time)))
        stopf("Exactly one parameter of 'secs' and 'stop_time' has to be set!")

      if (!is.null(pv$secs)) {
        d = difftime(Sys.time(), inst$start_time, units = "secs")
        return(d >= pv$secs)
      }
      return(Sys.time() >= pv$stop_time)
    }
  )
)

mlr_terminators$add("clock_time", TerminatorClockTime)
