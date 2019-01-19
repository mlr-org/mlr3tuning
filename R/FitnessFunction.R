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
#' ff = FitnessFunction$new(task, learner, resampling, param_set, 
#'   ctrl = tune_control())
#'
#' # Public members
#' ff$task
#' ff$learner
#' ff$resampling
#' ff$param_set
#' ff$ctrl
#' ff$hooks
#' ff$bmr
#' 
#' # Public methods
#' ff$eval(x)
#' ff$eval_vectorized(xts)
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
#' * `$new()` creates a new object of class [FitnessFunction].
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
#' * `$run_hooks()` run a function that runs on the whole `FitnessFunction` object.
#'
#' @name FitnessFunction
#' @keywords internal
#' @family FitnessFunction
#' @examples
#' # Object required to define the fitness function:
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' measures = mlr3::mlr_measures$mget("classif.mmce")
#' task$measures = measures
#' param_set = paradox::ParamSet$new(params = list(
#'   paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   paradox::ParamInt$new("minsplit", lower = 1, upper = 10)))
#' 
#' ff = FitnessFunction$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   param_set = param_set
#' )
#' 
#' ff$eval(data.frame(cp = 0.05, minsplit = 5))
#' ff$eval(data.frame(cp = 0.01, minsplit = 3))
#' ff$get_best()
NULL

#' @export
FitnessFunction = R6Class("FitnessFunction",
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
      self$hooks = list(update_start = list(), update_end = list())
    },

    eval = function(dt) {
      dt = data.table::as.data.table(dt)
      checkmate::assert_data_table(dt, any.missing = FALSE, min.rows = 1, min.cols = 1)
      self$eval_design(paradox::Design$new(self$param_set, dt))
    },

    eval_design = function(design) {
      checkmate::expect_r6(design, "Design")

      # Not that pretty but enables the use of transpose from Design:
      if (self$param_set$has_trafo) 
        design$data = self$param_set$trafo(design$data)

      learners = imap(design$transpose(), function(xt, i) {
        learner = self$learner$clone()
        learner$param_vals = insert_named(learner$param_vals, xt)
        learner$id = paste0(learner$id, i)
        return(learner)
      })

      self$run_hooks("update_start")
      
      bmr = mlr3::benchmark(design = data.table::data.table(task = list(self$task), learner = learners, 
        resampling = list(self$resampling)), ctrl = self$ctrl)

      # add params to benchmark result data. if statement ensures that map with just one learner returns 
      # the same as map with more than one learner: 
      bmr$data$pars = mlr3misc::map(bmr$data$learner, function (l) {
        if (nrow(bmr$data) == 1) {
          list(l$param_vals)
        } else {
          l$param_vals
        }
      })

      if (is.null(self$bmr)) {
        bmr$data$dob = 1L
        self$bmr = bmr
      } else {
        bmr$data$dob = max(self$bmr$data$dob) + 1L
        self$bmr$combine(bmr)
      }
      self$run_hooks("update_end")
      invisible(self)
    },

    get_best = function() {
      self$bmr$get_best(self$task$measures[[1L]])
    },

    run_hooks = function(id) {
      funs = self$hooks[[id]]
      for (fun in funs)
        do.call(fun, list(ff = self))
    }
  )
)
