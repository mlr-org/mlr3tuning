#' @title FitnessFunction
#'
#' @description
#' FitnessFunction.
#'
#' @section Usage:
#' ```
#' l = FitnessFunction(id)
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
    measure = NULL,
    param_set = NULL,
    terminator = NULL,
    additional_measures = NULL,

    experiment_store = NULL,
  
    initialize = function(task, learner, resampling, measure = NULL, param_set, terminator, additional_measures = NULL) {
      self$task = mlr3:::assert_task(task)
      self$learner = mlr3:::assert_learner(learner, task = task)
      self$resampling = mlr3:::assert_resampling(resampling)
      if (!is.null(measure)) {
        self$measure = mlr3:::assert_measure(measure, task = task, learner = learner)  
      } else {
        self$measure = task$measures[[1]]
      }
      self$param_set = assert_class(param_set, "ParamSet")
      self$terminator = assert_class(terminator$copy(), "TerminatorBase")
      if (!is.null(additional_measures)) {
        additional_measures = mlr3:::assert_measures(additional_measures, task = task, learner = learner)
        if (!is.null(measure)) {
          additional_measures = c(additional_measures, task$measures[[-1]])
        } else {
          additional_measures = c(additional_measures, task$measures)
        }
        self$additional_measures = additional_measures
      }
    },

    eval = function(x) {
      res = self$eval_vectorized(list(x))
      res[1, , drop = TRUE]
    },

    eval_vectorized = function(xs) {
      self$terminator$update_start(self)
      if (self$terminator$terminated) {
        stop(paste("Terminator: ", self$terminator$message))
      }
      learners = lapply(xs, function(x) {
        learner = self$learner$copy()
        learner$par_vals = mlr3:::insert.list(learner$par_vals, x)
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
      perfs = bmr$performance[, list(y = mean(get(self$measure$id))), by = "hash"]
      if (self$measure$minimize) {
        hash = perfs$hash[which.min(perfs$y)]
      } else {
        hash = perfs$hash[which.max(perfs$y)]
      }
      bmr$resample_result(hash)
    }
    
  ),
  active = list(),
  private = list()
)