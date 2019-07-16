#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have.
#' A tuner is an object that describes the tuning strategy how to search the hyperparameter space given within
#' the `[PerformanceEvaluator]` object.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(id, pe, terminator, settings = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Name of the tuner.
#' * `pe` :: [PerformanceEvaluator].
#' * `terminator` :: [Terminator].
#' * `settings` :: named `list()`\cr
#'   Arbitrary list, depending on the child class.
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr
#'   Name of the tuner.
#' * `pe` :: [PerformanceEvaluator]\cr
#'   The current state of the [PerformanceEvaluator].
#' * `terminator` :: [Terminator].
#'   The current state of the [Terminator].
#' * `settings` :: named `list()`\cr
#'   Arbitrary list, depending on the child class.
#'
#' @section Methods:
#' * `tune()`\cr
#'   () -> `self`\cr
#'   Performs the tuning until the budget of the [Terminator] is exhausted.
#' * `tune_result()`\cr
#'   () -> named `list()`\cr
#'   List with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `values` (named `list()`) with the corresponding hyperparameters values.
#' * `aggregate(unnest = TRUE)`
#'   `logical(1)` -> [data.table::data.table()]\cr
#'   Returns a table of resample results, similar to the one returned by [mlr3::benchmark()]'s `aggregate()` method.
#'   If `unnest` is `TRUE`, hyperparameter settings are stored in separate columns instead of inside a list column
#'
#' @family Tuner
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' resampling = mlr_resamplings$get("cv", param_vals = list(folds = 2))
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' pe = PerformanceEvaluator$new("iris", "classif.rpart", resampling, "classif.ce", param_set)
#' terminator = TerminatorEvaluations$new(3)
#' tt = TunerRandomSearch$new(pe, terminator) # swap this line to use a different Tuner
#' tt$tune()
#' tt$tune_result() # returns best configuration and performance
#' tt$aggregate() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    id = NULL,
    pe = NULL,
    terminator = NULL,
    settings = NULL,

    initialize = function(id, pe, terminator, settings = list()) {
      self$id = assert_string(id)
      self$pe = assert_r6(pe, "PerformanceEvaluator")
      self$terminator = assert_r6(terminator, "Terminator")
      self$settings = assert_list(settings, names = "unique")
    },

    tune = function() {
      while (!self$terminator$terminated) {
        # Catch exception when terminator is terminated:
        tryCatch(private$tune_step(), terminated_message = function(cond) { })
      }
      return(invisible(self))
    },

    tune_result = function() {
      measures = self$pe$measures
      rr = self$pe$bmr$best(measures[[1L]])
      list(performance = rr$aggregate(measures), values = rr$learners[[1L]]$param_set$values)
    },

    aggregate = function(unnest = TRUE) {
      if (is.null(self$pe$bmr)) {
        stopf("No tuning conducted yet.")
      }
      dt = self$pe$bmr$aggregate(params = TRUE)
      if (unnest) {
        dt = mlr3misc::unnest(dt, "params")
      }

      return(dt)
    }
  ),
  private = list(
    eval_design_terminator = function(design) {
      lg$info("Evaluating %i configurations", nrow(design$data))
      self$terminator$update_start(self$pe)
      self$pe$eval_design(design)
      self$terminator$update_end(self$pe)

      lg$info("Evaluation finished. Remaining: %s.", self$terminator$remaining)

      # Train as long as terminator is not terminated, if he is terminated throw condition of
      # class "terminated_message" that is caught by tryCatch.
      # The exception should be automatically caught since the while loop checks for itself
      # if the terminator is terminated.
      if (self$terminator$terminated) {
        stop(messageCondition("Termination criteria is reached", class = "terminated_message"))
      }
    }
  )
)
