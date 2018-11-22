#' @title FitnessFunction
#'
#' @description
#' Implements a fitness function for \pkg{mlr3}.
#' Input are hyperparameters, output is the predictive performance.
#'
#' @section Usage:
#' ```
#' ff = FitnessFunction(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the FitnessFunction.
#' * `learner` (`Learner`):
#'   The Learner that we want to evaluate.
#' * `resampling` (`Resampling`):
#'   The Resampling method that is used to obtain the y value.
#' * `measure` (`Measure`):
#'   Optional, can override the Measure in the Task
#' * `param_set` ([paradox::ParamSet]):
#'   Parameter Set.
#' * `tune_control` (`list()`):
#'   See [tune_control()].
#'
#' @section Details:
#' * `$new()` creates a new object of class [FitnessFunction].
#' * `$eval(x)` (`numeric(length(self$measures))`) evaluates the parameter setting `x` (`list`) for the given learner and resampling.
#' * `$eval_vectorized(xs)` (`matrix(length(xs), length(self$measures))`) performs resampling for multiple parameter settings `xs` (list of lists).
#'
#' @name FitnessFunction
#' @keywords internal
#' @family FitnessFunction
NULL

#' @export
FitnessFunction = R6Class("FitnessFunction",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    ctrl = NULL,
    hooks = NULL,
    experiments = NULL,

    initialize = function(task, learner, resampling, measures = NULL, param_set, ctrl = tune_control()) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
      self$measures = mlr3::assert_measures(measures %??% task$measures, task = task, learner = learner)
      self$param_set = assert_class(param_set, "ParamSet")
      self$ctrl = assert_list(ctrl, names = "unique")
      self$experiments = data.table()
      self$hooks = list(update_start = list(), update_end = list())
    },

    rbind = function(experiments) {
      if (nrow(self$experiments) == 0L) {
        experiments$dob = 1L
        self$experiments = experiments
      } else {
        experiments$dob = self$experiments[, max(dob)] + 1L
        self$experiments = rbind(self$experiments, experiments)
      }
    },

    eval = function(x) {
      self$eval_vectorized(list(x))
    },

    eval_vectorized = function(xs) {
      learners = lapply(xs, function(x) {
        learner = self$learner$clone()
        learner$param_vals = insert_named(learner$param_vals, x)
        return(learner)
      })

      self$run_hooks("update_start")
      bmr = mlr3::benchmark(tasks = list(self$task), learners = learners, resamplings = list(self$resampling), measures = self$measures, ctrl = self$ctrl)
      self$rbind(bmr$data)
      self$run_hooks("update_end")
      invisible(self)
    },

    get_best = function() {
      if (nrow(self$experiments) == 0L)
        stop("No experiments conducted")
      bmr = mlr3::BenchmarkResult$new(self$experiments)
      m = self$measures[[1L]]
      perfs = bmr$aggregated
      bmr$resample_result(perfs$hash[which_best(m, perfs[[m$id]])])
    },

    run_hooks = function(id) {
      funs = self$hooks[[id]]
      for (fun in funs)
        do.call(fun, list(ff = self))
    }
  )
)

which_best = function(measure, x) {
  if (measure$minimize) which_min(x) else which_max(x)
}
