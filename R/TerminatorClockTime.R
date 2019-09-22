#' @title Terminator that stops according to the clock time
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
#' TerminatorClockTime$new()
#' term("clock_time")
#' ```
#'
#' @section Parameters:
#' * `secs` :: `numeric(1)`\cr
#'   Maximum allowed time, in seconds, default is 100.
#'   Mutually exclusive with argument `stop_time`.
#'
#' * `stop_time` :: `POSIXct(1)`\cr
#'   Terminator stops after this point in time.
#'   Mutually exclusive with argument `secs`.
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
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("secs", lower = 0, default = 100),
        ParamUty$new("stop_time"))
      )
      super$initialize(param_set = ps)
    },

    is_terminated = function(instance) {
      pv = self$param_set$values
      #FIXME: actually this should be done in the assert of the paramset?
      if (!xor(is.null(pv$secs), is.null(pv$stop_time)))
        stopf("Exactly one parameter of 'secs' and 'stop_time' can be set!")

      if (!is.null(pv$secs)) {
        d = difftime(Sys.time(), instance$start_time, units = "secs")
        return(d >= pv$secs)
      }
      return(Sys.time() >= pv$stop_time)
    }
  )
)

mlr_terminators$add("clock_time", TerminatorClockTime)
