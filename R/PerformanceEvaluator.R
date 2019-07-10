#' @title PerformanceEvaluator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Implements a performance evaluator for \pkg{mlr3} as `R6` class `PerformanceEvaluator`.
#' An object of that class contains all relevant information that is necessary to conduct tuning.
#'
#' After defining a performance evaluator, we can use it to predict the generalization error of a specific learner configuration
#' defined by its hyperparameters (using the method `$eval()`).
#' The `PerformanceEvaluator` class is the basis for further tuning strategies, i.e., grid or random search.
#'
#' @section Construction:
#' ```
#' pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set,
#'   ctrl = list())
#'```
#'
#' * `task` :: [mlr3::Task].
#' * `learner` :: [mlr3::Learner].
#' * `resampling` :: [mlr3::Resampling].
#' * `measures` :: list of [mlr3::Measure].
#' * `param_set` :: [paradox::ParamSet].
#' * `ctrl` :: named `list()`\cr
#'   See [mlr3::mlr_control()].
#'
#' @section Fields:
#' * `task` :: [mlr3::Task]\cr
#'   Stored task.
#' * `learner` :: [mlr3::Learner]\cr
#'   Stored learner.
#' * `resampling` :: [mlr3::Resampling]\cr
#'   Stored resampling
#' * `measures` :: list of [mlr3::Measure]\cr
#'   Stored measures.
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Stored parameter set.
#' * `bmr` :: [mlr3::BenchmarkResult]\cr
#'   A benchmark result, which is used as data storage.
#' * `hooks` :: `list()`\cr
#'   List of functions that are executed with `run_hooks()` for evaluation.
#'   This is for internal use.
#'
#' @section Methods:
#' * `eval(dt)`\cr
#'   [data.table::data.table()] -> `self`\cr
#'   Evaluates all hyperparameter configurations in `dt`.
#'   Each configuration is a row.
#' * `eval(design)`\cr
#'   [paradox::Design] -> `self`\cr
#'   Evaluates all configurations defined by the design.
#' * `best()`\cr
#'   () -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according to the
#'   first measure in `$measures`.
#' * `run_hooks()`\cr
#'   `()` -> `NULL`\cr
#'   Runs all hook functions. For internal use.
#' * `add_hook(hook)`\cr
#'   `function()` -> `NULL`\cr
#'   Adds a hook function. For internal use.
#'
#'
#' @family PerformanceEvaluator
#' @export
#' @examples
#' # Object required to define the performance evaluator:
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' param_set = paradox::ParamSet$new(params = list(
#'   paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   paradox::ParamInt$new("minsplit", lower = 1, upper = 10)))
#'
#' pe = PerformanceEvaluator$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   param_set = param_set
#' )
#'
#' pe$eval(data.table::data.table(cp = 0.05, minsplit = 5))
#' pe$eval(data.table::data.table(cp = 0.01, minsplit = 3))
#' pe$best()
PerformanceEvaluator = R6Class("PerformanceEvaluator",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    ctrl = NULL,
    hooks = NULL,
    bmr = NULL,

    initialize = function(task, learner, resampling, measures, param_set, ctrl = list()) {
      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = task)
      self$resampling = assert_resampling(resampling)
      self$measures = assert_measures(measures)
      self$param_set = assert_class(param_set, "ParamSet")
      self$ctrl = mlr_control(ctrl)
    },

    eval = function(dt) {
      assert_data_table(dt, any.missing = FALSE, min.rows = 1, min.cols = 1)
      self$eval_design(paradox::Design$new(self$param_set, dt, remove_dupl = FALSE))
    },

    # evaluates all points in a design
    # possibly transforms the data before using the trafo from self$param set
    eval_design = function(design) {
      assert_r6(design, "Design")

      # Not that pretty but enables the use of transpose from Design:
      if (self$param_set$has_trafo) {
        design$data = self$param_set$trafo(design$data)
      }

      n_evals = if (is.null(self$bmr)) 0L else self$bmr$data[, data.table::uniqueN(get("hash"))]

      learners = imap(design$transpose(), function(xt, i) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, xt)
        return(learner)
      })

      bmr = mlr3::benchmark(mlr3::expand_grid(tasks = list(self$task), learners = learners,
        resamplings = list(self$resampling)), ctrl = self$ctrl)

      if (is.null(self$bmr)) {
        self$bmr = bmr
      } else {
        self$bmr$combine(bmr)
      }

      self$run_hooks()
      invisible(self)
    },

    best = function() {
      self$bmr$best(self$measures[[1L]])
    },

    add_hook = function(hook) {
      assert_function(hook, args = "pe", nargs = 1)
      self$hooks = append(self$hooks, hook)
    },

    run_hooks = function() {
      lapply(self$hooks, function(hook) hook(pe = self))
    }
  )
)
