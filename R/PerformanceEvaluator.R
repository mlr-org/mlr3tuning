#' @title PerformanceEvaluator Class
#'
#' @description
#' Implements a performance evaluator for \pkg{mlr3} as `R6` class `PerformanceEvaluator`. An object of that class
#' contains all relevant informations that are necessary to conduct tuning (`mlr3::Task`, `mlr3::Learner`, `mlr3::Resampling`, `mlr3::Measure`s,
#' `paradox::ParamSet`).
#' After defining a performance evaluator, we can use it to predict the generalization error of a specific learner configuration
#' defined by it's hyperparameter (using `$eval()`).
#' The `PerformanceEvaluator` class is the basis for further tuning strategies, i.e., grid or random search.
#'
#' @section Usage:
#' ```
#' # Construction
#' pe = PerformanceEvaluator$new(task, learner, resampling, param_set,
#'   ctrl = tune_control())
#'
#' # Public members
#' pe$task
#' pe$learner
#' pe$resampling
#' pe$param_set
#' pe$ctrl
#' pe$hooks
#' pe$bmr
#'
#' # Public methods
#' pe$eval(x)
#' pe$eval_vectorized(xts)
#' pe$get_best()
#' pe$run_hooks(id)
#' ```
#'
#' @section Arguments:
#' * `task` (`mlr3::Task`):
#'   The task that we want to evaluate.
#' * `learner` (`mlr3::Learner`):
#'   The learner that we want to evaluate.
#' * `resampling` (`mlr3::Resampling`):
#'   The Resampling method that is used to evaluate the learner.
#' * `param_set` ([paradox::ParamSet]):
#'   Parameter set to define the hyperparameter space.
#' * `ctrl` (`list()`):
#'   See [tune_control()].
#' * `xt` (`list()`):
#'   A specific (transformed) parameter configuration given as named list (e.g. for rpart `list(cp = 0.05, minsplit = 4)`).
#' * `xts` (`list()`):
#'   Collection of multiple (transformed) parameter values gained that is, for example, gained from a tuning strategy like grid search (see `?paradox::generate_design_grid`).
#' * `id` (`character(1)`):
#'   Identifier of a hook.
#'
#' @section Details:
#' * `$new()` creates a new object of class [PerformanceEvaluator].
#' * `$task` (`mlr3::Task`) the task for which the tuning should be conducted.
#' * `$learner` (`mlr3::Learner`) the algorithm for which the tuning should be conducted.
#' * `$resampling` (`mlr3::Resampling`) strategy to evaluate a parameter setting
#' * `$param_set` (`paradox::ParamSet`) parameter space given to the `Tuner` object to generate parameter values.
#' * `$ctrl` (`list()`) execution control object for tuning (see `?tune_control`).
#' * `$hooks` (`list()`) list of functions that could be executed with `run_hooks()`.
#' * `$bmr` (`mlr3::BenchmarkResult`) object that contains all tuning results as `BenchmarkResult` object (see `?BenchmarkResult`).
#' * `$eval(xt)` evaluates the (transformed) parameter setting `xt` (`list`) for the given learner and resampling.
#' * `$eval_vectorized(xts)` performs resampling for multiple (transformed) parameter settings `xts` (list of lists).
#' * `$get_best()`  get best parameter configuration from the `BenchmarkResult` object.
#' * `$run_hooks()` run a function that runs on the whole `PerformanceEvaluator` object.
#'
#' @name PerformanceEvaluator
#' @keywords internal
#' @family PerformanceEvaluator
#' @examples
#' # Object required to define the performance evaluator:
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' task$measures = measures
#' param_set = paradox::ParamSet$new(params = list(
#'   paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   paradox::ParamInt$new("minsplit", lower = 1, upper = 10)))
#'
#' pe = PerformanceEvaluator$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   param_set = param_set
#' )
#'
#' pe$eval(data.table::data.table(cp = 0.05, minsplit = 5))
#' pe$eval(data.table::data.table(cp = 0.01, minsplit = 3))
#' pe$get_best()
NULL

#' @export
PerformanceEvaluator = R6Class("PerformanceEvaluator",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    param_set = NULL,
    ctrl = NULL,
    hooks = NULL,
    bmr = NULL,

    initialize = function(task, learner, resampling, param_set, ctrl = tune_control()) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
      self$param_set = checkmate::assert_class(param_set, "ParamSet")
      self$ctrl = checkmate::assert_list(ctrl, names = "unique")
    },

    eval = function(dt) {
      checkmate::assert_data_table(dt, any.missing = FALSE, min.rows = 1, min.cols = 1)
      self$eval_design(paradox::Design$new(self$param_set, dt, remove_dupl = FALSE))
    },

    eval_design = function(design) {

      checkmate::assert_r6(design, "Design")

      # Not that pretty but enables the use of transpose from Design:
      if (self$param_set$has_trafo) {
        design$data = self$param_set$trafo(design$data)
      }

      n_evals = if (is.null(self$bmr)) 0 else nrow(self$bmr$aggregated())

      learners = imap(design$transpose(), function(xt, i) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, xt)
        learner$id = paste0(learner$id, n_evals + i)
        return(learner)
      })

      bmr = mlr3::benchmark(mlr3::expand_grid(tasks = list(self$task), learners = learners,
        resamplings = list(self$resampling)), ctrl = self$ctrl)

      # add params to benchmark result data. if statement ensures that map with just one learner returns
      # the same as map with more than one learner:
      bmr$data$pars = map(bmr$data$learner, function(l) {
        if (nrow(bmr$data) == 1) {
          list(l$param_set$values)
        } else {
          l$param_set$values
        }
      })
      if (is.null(self$bmr)) {
        bmr$data$dob = 1L
        self$bmr = bmr
      } else {
        bmr$data$dob = max(self$bmr$data$dob) + 1L
        self$bmr$combine(bmr)
      }
      self$run_hooks()
      invisible(self)
    },

    get_best = function() {
      self$bmr$get_best(self$task$measures[[1L]]$id)
    },

    add_hook = function(hook) {
      checkmate::assert_function(hook, args = "pe", nargs = 1)
      self$hooks = append(self$hooks, hook)
    },

    run_hooks = function() {
      lapply(self$hooks, function(hook) {
        do.call(hook, list(pe = self))
      })
    },
    deep_clone = function(name, value) {
      if (R6::is.R6(value)) {
        value$clone(deep = TRUE)
      } else {
        value
      }
    }
  )
)
