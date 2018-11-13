#' @title TunerBase
#'
#' @description
#' TunerBase.
#'
#' @section Usage:
#' ```
#' tuner = TunerBase(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#' * `settings` (`list`):
#'   The settings for the Tuner.
#' * `terminator` (`Terminator`).
#'   All tuning problems optimized with this Tuner object will be terminated by this terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerBase].
#'
#' @name TunerBase
#' @keywords internal
#' @family Tuner
NULL

#' @export
TunerBase = R6Class("TunerBase",
  public = list(

    id = NULL,
    ff = NULL,
    settings = NULL,

    initialize = function(id, ff, settings) {
      self$id = assert_string(id)
      self$ff = assert_class(ff, "FitnessFunction")
      self$settings = assert_list(settings)
    },

    tune = function(task, learner, param_set) {
      stop("tune() not implemented for TunerBase.")
    },

    tune_step = function() {
      stop("tune_step() not implemented for TunerBase.")
    },

    tune_result = function() {
      self$ff$get_best()
    }
  )
)
