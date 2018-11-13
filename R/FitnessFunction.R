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
#'
#' @section Details:
#' `$new()` creates a new object of class [FitnessFunction].
#'
#' `$eval(x)` (`numeric(length(self$measures))`) evaluates the parameter setting `x` (`list`) for the given learner and resampling.
#'
#' `$eval_vectorized(xs)` (`matrix(length(xs), length(self$measures))`) performs resampling for multiple parameter settings `xs` (list of lists).
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
    ctrl = NULL,

    experiments = NULL,

    initialize = function(task, learner, resampling, measures = NULL, param_set, terminator, ctrl = mlr3::mlr_control()) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
      self$measures = mlr3::assert_measures(measures %??% task$measures, task = task, learner = learner)
      self$param_set = assert_class(param_set, "ParamSet")
      self$terminator = assert_class(terminator$clone(), "TerminatorBase")
      self$ctrl = assert_list(ctrl, names = "unique")
      self$experiments = data.table()
    },

    eval = function(x) {
      res = self$eval_vectorized(list(x))
      res[1L, , drop = TRUE]
    },

    eval_vectorized = function(xs) {
      if (self$terminator$terminated) {
        stop(paste("Terminator: ", self$terminator$message))
      }

      learners = lapply(xs, function(x) {
        learner = self$learner$clone()
        learner$param_vals = insert.list(learner$param_vals, x)
        return(learner)
      })

      self$terminator$update_start(self)
      bmr = benchmark(tasks = list(self$task), learners = learners, resamplings = list(self$resampling), measures = self$measures, ctrl = self$ctrl)
      self$terminator$update_end(self)

      if (nrow(self$experiments) == 0L) {
        self$experiments = bmr$data
      } else {
        self$experiments = rbind(self$experiments, bmr$data)
      }

      as.matrix(bmr$aggregated[, ids(self$measures), with = FALSE])
    },

    get_best = function() {
      if (nrow(self$experiments) == 0L)
        stop("No experiments conducted")
      bmr = mlr3::BenchmarkResult$new(self$experiments)
      m = self$measures[[1L]]
      perfs = bmr$aggregated


      if (m$minimize) {
        hash = perfs$hash[which.min(perfs[[m$id]])]
      } else {
        hash = perfs$hash[which.max(perfs[[m$id]])]
      }
      bmr$resample_result(hash)
    }
  )
)
