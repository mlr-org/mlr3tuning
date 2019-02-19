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
#' tuner$aggregated(unnest)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   The id of the Tuner.
#' * `ff` (`[FitnessFunction]`).
#' * `terminator` (`[Terminator]`).
#' * `settings` (`list`):\cr
#'   The settings for the Tuner.
#' * `unnest` (`logical(1)`):\cr
#'   If TRUE returns each parameter as a column of the data table. Otherwise the column includes a list of the parameter.
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
#'     - `param_set$values` (`numeric()`) with corresponding hyperparameters.
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
    },

    tune = function() {
      while (! self$terminator$terminated) {
        # Catch exception when terminator is terminated:
        tryCatch(private$tune_step(), .terminated_message = function (cond) { })
      }
    },

    tune_result = function() {
      measure = self$ff$task$measures[[1L]]
      rr = self$ff$bmr$get_best(measure)
      list(performance = rr$aggregated, values = rr$learner$param_set$values)
    },

    aggregated = function(unnest = TRUE) {
      if (!is.null(self$ff$bmr)) {
        dt = self$ff$bmr$aggregated()
        dt$pars = mlr3misc::map(dt[["learner"]], function (l) l$param_set$values)
        if (unnest)
          dt = mlr3misc::unnest(dt, "pars")

        # [] forces the data table to get printed. This is suppressed by the first call of dt after
        # using := within []
        return(dt[])
      } else {
        mlr3misc::stopf("No tuning conducted yet.")
      }
    }
  ),
  private = list(
    eval_design_terminator = function (design) {
      self$terminator$update_start(self$ff)
      self$ff$eval_design(design)
      self$terminator$update_end(self$ff)

      # Train as long as terminator is not terminated, if he is terminated throw condition of
      # class ".terminated_message" that is caught by tryCatch.
      # The exception should be automatically caught since the while loop checks for itself
      # if the terminator is terminated.
      if (self$terminator$terminated)
        stop(conditions::condition_message(".terminated", "Termination criteria is reached"))
    },
    deep_clone = function (name, value) {

      if (R6::is.R6(value)) {
        value$clone(deep = TRUE)
      } else {
        value
      }
    }
  )
)
