#' @title Abstract Tuner Class
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have. A tuner is 
#' an object that describes the tuning strategy how to search the hyperparameter space given within
#' the `[FitnessFunction]` object. 
#'
#' @section Usage:
#' ```
#' # Construction
#' tuner = Tuner$new(id, ff, terminator, settings = list())
#' 
#' # public members
#' tuner$id
#' tuner$ff
#' tuner$terminator
#' tuner$settings
#' 
#' # public methods
#' tuner$tune()
#' tuner$tune_result()
#' 
#' # active bindings
#' tuner$aggregated
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   The id of the Tuner.
#' * `ff` (`[FitnessFunction]`).
#' * `terminator` (`[Terminator]`).
#' * `settings` (`list`):\cr
#'   The settings for the Tuner.
#'
#' @section Details:
#' * `$new()` creates a new object of class `[Tuner]`.
#' * `id` stores an identifier for this `[Tuner]`.
#' * `ff` stores the [FitnessFunction] to optimize.
#' * `terminator` stores the `[Terminator]`.
#' * `settings` is a list of hyperparamter settings for this `[Tuner]`.
#' * `tune()` performs the tuning, until the budget of the `[Terminator]` in the `[FitnessFunction]` is exhausted.
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
    terminator = NULL,
    settings = NULL,

    initialize = function(id, ff, terminator, settings = list()) {
      self$id = assert_string(id)
      self$ff = assert_r6(ff, "FitnessFunction")
      self$terminator = assert_r6(terminator, "Terminator")
      self$settings = assert_list(settings, names = "unique")

      ff$hooks$update_start = c(ff$hooks$update_start, list(terminator$update_start))
      ff$hooks$update_end = c(ff$hooks$update_end, list(terminator$update_end))
    },

    tune = function() {
      while (!self$terminator$terminated) {
        private$tune_step()
      }
      invisible(self)
    },

    tune_result = function() {
      measure = self$ff$task$measures[[1L]]
      rr = self$ff$bmr$get_best(measure)
      list(performance = rr$aggregated, param_vals = rr$learner$param_vals)
    },

    aggregate = function(unnest = TRUE) {
      if (! is.null(self$ff$bmr)) {
        dt = self$ff$bmr$aggregated
          
        # Get unique hashes with corresponding params:
        dt_pars = self$ff$bmr$data[, c("hash", "pars")]
        dt_pars = dt_pars[, .SD[1], "hash"]
          
        # Merge params to aggregated:
        dt[dt_pars, on = "hash", pars := i.pars]
        if (unnest)
          dt = mlr3misc::unnest(dt, "pars")

        # [] forces the data table to get printed. This is suppressed by the first call of dt after
        # using := within []
        return(dt[])
      } else {
        mlr3misc::stopf("No tuning conducted yet.")
      }
    }
  )
)
