#' @title TunerGenSA
#'
#' @description
#' Tuner child class to conduct generalized simulated annealing (GenSA) as tuning technique.
#'
#' @section Usage:
#' ```
#' tuner = TunerGenSA$new(pe, terminator, ...)
#' ```
#' See [Tuner] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` ([PerformanceEvaluator]):
#'   Black box function used for the tuning.
#' * `terminator` ([Terminator]):
#'   Terminator to control the termination.
#'   Will be triggered by the hooks of the [PerformanceEvaluator].
#' * `...`:
#'   Additional arguments passed to [GenSA::GenSA].
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerGenSA].
#' The interface is described in [Tuner].
#'
#' @name TunerGenSA
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' resampling$param_set$values$folds = 2
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' param_set = paradox::ParamSet$new(
#'   params = list(
#'     paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#'   )
#' )
#' pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
#'
#' terminator = TerminatorEvaluations$new(10)
#' rs = TunerGenSA$new(pe, terminator)
#' rs$tune()$tune_result()
NULL

#' @export
#' @include Tuner.R
TunerGenSA = R6Class("TunerGenSA",
  inherit = Tuner,
  public = list(
    initialize = function(pe, terminator, ...) {
      if (any(pe$param_set$storage_type != "numeric")) {
        err_msg = "Parameter types needs to be numeric"
        lg$error(err_msg)
        stopf(err_msg)
      }

      # Default settings:
      settings = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      super$initialize(id = "GenSA", pe = pe, terminator = terminator, settings = insert_named(settings, list(...)))
    }
  ),
  private = list(
    tune_step = function() {
      objective = function(x, pe) {
        measure = pe$measures[[1L]]

        hashes = pe$bmr$data$hash
        params = setDT(as.list(x))
        setnames(params, pe$param_set$ids())
        private$eval_design_terminator(paradox::Design$new(pe$param_set, params, remove_dupl = TRUE))
        new_hash = setdiff(pe$bmr$data$hash, hashes)

        perf = pe$bmr$resample_result(new_hash)$aggregate(measure)
        if (measure$minimize) perf else -perf
      }
      GenSA::GenSA(fn = objective, lower = self$pe$param_set$lower, upper = self$pe$param_set$upper,
        control = self$settings, pe = self$pe)
    }
  )
)
