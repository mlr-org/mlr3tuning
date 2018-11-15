#' @title Tuner Base Class
#'
#' @description
#' TunerBase.
#'
#' @section Usage:
#' ```
#' tuner = Tuner$new(id)
#' # public members
#' tuner$id
#' tuner$ff
#' tuner$settings
#' # public methods
#' tuner$tune()
#' tuner$tune_result()
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#' * `settings` (`list`):
#'   The settings for the Tuner.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Tuner].
#' * `id` stores an identifier for this [Tuner].
#' * `ff` stores the [FitnessFunction] to optimize.
#' * `settings` is a list of hyperparamter settings for this [Tuner].
#' * `tune()` performs the tuning, until the budget of the [Terminator] in the [FitnessFunction] is exhausted.
#' * `tune_result()` returns a list with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `param_vals` (`numeric()`) with corresponding hyperparameters.
#' @name Tuner
#' @family Tuner
NULL

#' @export
Tuner = R6Class("Tuner",
  public = list(
    id = NULL,
    ff = NULL,
    settings = NULL,

    initialize = function(id, ff, settings) {
      self$id = assert_string(id)
      self$ff = assert_class(ff, "FitnessFunction")
      self$settings = assert_list(settings)
    },

    tune = function() {
      while (!self$ff$terminator$terminated) {
        private$tune_step()
      }
      invisible(self)
    },

    tune_result = function() {
      rr = self$ff$get_best()
      list(performance = rr$aggregated, param_vals = rr$learner$param_vals)
    }
  )
)
