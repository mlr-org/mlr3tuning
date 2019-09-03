#' @title Terminator that stops after a budget of model evaluation time is depleted
#'
#' @aliases mlr_terminators_model_time
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning after a given model evaluation budget is exceeded.
#' The terminator measures the used time to train and predict all models contained
#' in the archive.
#'
#' @section Construction:
#' ```
#' t = TerminatorModelTime$new(secs)
#' ```
#'
#' * `secs` :: `numeric(1)`\cr
#'   Maximum allowed time, in seconds.
#'   Stored in `settings`.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorModelTime$new(3)
#' term("model_time", secs = 10 * 3600)
TerminatorModelTime = R6Class("TerminatorModelTime",
  inherit = Terminator,
  public = list(
    initialize = function(secs) {
      assert_number(secs, lower = 0)
      super$initialize(settings = list(secs = secs))
    },

    is_terminated = function(inst) {
      if (is.null(inst$bmr))
        return(FALSE)
      # extract train and predict timings and sum them up
      t_all = sum(map_dbl(inst$bmr$data$learner, function(x) sum(x$timings)))
      return(t_all >= self$settings$secs)
    }
  )
)

mlr_terminators$add("model_time", TerminatorModelTime)
