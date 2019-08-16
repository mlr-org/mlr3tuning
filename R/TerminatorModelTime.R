#' @title Terminator that stops after a budget of model evaluation time is depleted
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
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
#' TerminatorModelTime$new(10 * 3600)
TerminatorModelTime = R6Class("TerminatorModelTime",
  inherit = Terminator,
  public = list(
    initialize = function(secs) {
      assert_number(secs, lower = 0)
      super$initialize(settings = list(secs = secs))
    },

    is_terminated = function(pe) {
      if (is.null(pe$bmr))
        return(FALSE)
      # extract train and predict timings and sum them up
      t_all = sum(map_dbl(pe$bmr$data$learner, function(x) sum(x$timings)))
      return(t_all >= self$settings$secs)
    }
  )
)
