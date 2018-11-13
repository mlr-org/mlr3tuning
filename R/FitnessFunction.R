#' @title FitnessFunction
#'
#' @description
#' FitnessFunction.
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
#' * `param_set` (`ParamSet`):
#'   Optional, can override the ParamSet of the Learner.
#'
#' @section Details:
#' `$new()` creates a new object of class [FitnessFunction].
#'
#' `$eval(x)` (`numeric(length(self$measures))`) evaluates the parameter setting `x` (`list`) for the given learner and resampling.
#'
#' `$eval_vectorized(xs)` (`matrix(length(xs), length(self$measures))`) performs resampling for mulitple parameter settings `xs` (list of lists).
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
    terminator = NULL,

    experiment_store = NULL,

    initialize = function(task, learner, resampling, measures = NULL, param_set, terminator) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
      self$measures = mlr3::assert_measures(measures %??% task$measures, task = task, learner = learner)
      self$param_set = assert_class(param_set, "ParamSet")
      self$terminator = assert_class(terminator$clone(), "TerminatorBase")
    },

    eval = function(x) {
      res = self$eval_vectorized(list(x))
      res[1L, , drop = TRUE]
    },

    eval_vectorized = function(xs) {
      self$terminator$update_start(self)
      if (self$terminator$terminated) {
        stop(paste("Terminator: ", self$terminator$message))
      }
      learners = lapply(xs, function(x) {
        learner = self$learner$clone()
        learner$par_vals = mlr3::insert(learner$par_vals, x)
        return(learner)
      })
      res = benchmark(tasks = list(self$task), learners = learners, resamplings = list(self$resampling), measures = self$measures)
      if (is.null(self$experiment_store)) {
        self$experiment_store = res$data
      } else {
        self$experiment_store = rbind(self$experiment_store, res$data)
      }
      self$terminator$update_end(self)
      t(vapply(res$aggregated, identity, numeric(length(self$measures))))
    },

    get_best = function() {
      bmr = mlr3:::BenchmarkResult$new(self$experiment_store)
      m = self$measures[[1L]]
      perfs = bmr$performance[, list(y = mean(get(m$id))), by = "hash"]
      if (m$minimize) {
        hash = perfs$hash[which.min(perfs$y)]
      } else {
        hash = perfs$hash[which.max(perfs$y)]
      }
      bmr$resample_result(hash)
    }
  )
)
