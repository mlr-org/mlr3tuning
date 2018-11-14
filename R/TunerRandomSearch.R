#' @title TunerRandomSearch
#'
#' @description
#' TunerRandomSearch
#'
#' @section Usage:
#' ```
#' tuner = TunerRandomSearch(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerRandomSearch].
#'
#' @name TunerRandomSearch
#' @keywords internal
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' measures = mlr3::mlr_measures$mget("mmce")
#' terminator = TerminatorEvaluations$new("term-evals", 10)
#' param_set = paradox::ParamSet$new(params = list(paradox::ParamReal$new("cp", lower = 0.001, upper = 0.1)))
#'
#' ff = FitnessFunction$new(task, learner, resampling, measures, param_set, terminator)
#'
#' rs = TunerRandomSearch$new("rs", ff)
#' rs$tune(ps)
NULL

#' @export
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerBase,
  public = list(

    initialize = function(id, ff, batch_n = 100) {
      super$initialize(id = id, ff = ff, settings = list(batch_n = batch_n))
    },

    tune = function(param_set) {
      while (!self$ff$terminator$terminated) {
        self$tune_step()
      }

      rr = self$ff$get_best()
      list(
        performance = rr$aggregated,
        param_vals = rr$learner$param_vals
      )
    },

    tune_step = function() {
      n = min(self$settings$batch_n, self$ff$terminator$remaining)
      xs = self$ff$param_set$sample(n)
      xs = self$ff$param_set$transform(xs)
      y = self$ff$eval_vectorized(.mapply(list, xs, list()))
      invisible(y)
    }
  )
)
