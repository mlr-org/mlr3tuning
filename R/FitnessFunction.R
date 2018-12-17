#' @title FitnessFunction Class
#'
#' @description
#' Implements a fitness function for \pkg{mlr3} as `R6` class `FitnessFunction`. An object of that class
#' contains all relevant informations that are necessary to conduct tuning (`mlr3::Task`, `mlr3::Learner`, `mlr3::Resampling`, `mlr3::Measure`s,
#' `paradox::ParamSet`). 
#' After defining a fitness function, we can use it to predict the generalization error of a specific learner configuration
#' defined by it's hyperparameter (using `$eval()`). 
#' The `FitnessFunction` class is the basis for further tuning strategies, i.e., grid or random search. 
#'
#' @section Usage:
#' ```
#' # Construction
#' ff = FitnessFunction$new(task, learner, resampling, measures = NULL, param_set, 
#'   ctrl = tune_control())
#'
#' # Public members
#' ff$task
#' ff$learner
#' ff$resampling
#' ff$measures
#' ff$param_set
#' ff$ctrl
#' ff$hooks
#' ff$bmr
#' 
#' # Public methods
#' ff$eval(params)
#' ff$eval_vectorized(param_vals)
#' ff$get_best()
#' ff$run_hooks(id)
#' ```
#'
#' @section Arguments:
#' * `task` (`mlr3::Task`):
#'   The task that we want to evaluate.
#' * `learner` (`mlr3::Learner`):
#'   The learner that we want to evaluate.
#' * `resampling` (`mlr3::Resampling`):
#'   The Resampling method that is used to evaluate the learner.
#' * `measures` (`mlr3::Measure`):
#'   Optional, can override the Measure of the Task
#' * `param_set` ([paradox::ParamSet]):
#'   Parameter set to define the hyperparameter space.
#' * `ctrl` (`list()`):
#'   See [tune_control()].
#' * `params` (`list()`):
#'   A specific parameter configuration given as names list.
#' * `param_vals` (`list()`):
#'   Collection of multiple parameter values gained that is, for example, gained from a tuning strategy like grid search (see `?paradox::generate_design_grid`).
#' * `id` (`character(1)`):
#'   Identifier of a hook.
#'
#' @section Details:
#' * `$new()` creates a new object of class [FitnessFunction].
#' * `$task` (`mlr3::Task`) the task for which the tuning should be conducted.
#' * `$learner` (`mlr3::Learner`) the algorithm for which the tuning should be conducted.
#' * `$resampling` (`mlr3::Resampling`) strategy to evaluate a parameter setting
#' * `$measures` (`list(Measure)`) list of `mlr3::Measure` objects that are used for evaluation.
#' * `$param_set` (`paradox::ParamSet`) parameter space given to the `Tuner` object to generate parameter values.
#' * `$ctrl` (`list()`) execution control object for tuning (see `?tune_control`).
#' * `$hooks` (`list()`) list of functions that could be executed with `run_hooks()`.
#' * `$bmr` (`mlr3::BenchmarkResult`) object that contains all tuning results as `BenchmarkResult` object (see `?BenchmarkResult`).
#' * `$eval(params)` evaluates the parameter setting `params` (`list`) for the given learner and resampling.
#' * `$eval_vectorized(param_vals)` performs resampling for multiple parameter settings `param_vals` (list of lists).
#' * `$get_best()` performs resampling for multiple parameter settings `param_vals` (list of lists).
#' * `$run_hooks()` performs resampling for multiple parameter settings `param_vals` (list of lists).
#'
#' @name FitnessFunction
#' @keywords internal
#' @family FitnessFunction
#' @examples
NULL

#' @export
FitnessFunction = R6Class("FitnessFunction",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    ctrl = NULL,  # private?
    hooks = NULL, # private?
    bmr = NULL,

    initialize = function(task, learner, resampling, measures = NULL, param_set, ctrl = tune_control()) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
      self$measures = mlr3::assert_measures(measures %??% task$measures, task = task)
      self$param_set = assert_class(param_set, "ParamSet")
      self$ctrl = assert_list(ctrl, names = "unique")
      self$hooks = list(update_start = list(), update_end = list())
    },

    eval = function(params) {
      self$eval_vectorized(list(params))
    },

    eval_vectorized = function(param_vals) {
      learners = lapply(param_vals, function(params) {
        learner = self$learner$clone()
        learner$param_vals = insert_named(learner$param_vals, params)
        return(learner)
      })

      self$run_hooks("update_start")
      bmr = mlr3::benchmark(tasks = list(self$task), learners = learners, resamplings = list(self$resampling), measures = self$measures, ctrl = self$ctrl)
      self$bmr = if (is.null(self$bmr)) bmr else self$bmr$combine(bmr)
      self$run_hooks("update_end")
      invisible(self)
    },

    get_best = function() {
      self$bmr$get_best(self$measures[[1L]])
    },

    run_hooks = function(id) {
      funs = self$hooks[[id]]
      for (fun in funs)
        do.call(fun, list(ff = self))
    }
  )
)
