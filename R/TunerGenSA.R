#' @title TunerGenSA
#'
#' @description
#' Tuner child class to conduct generalized simulated annealing (GenSA) as tuning technique.
#'
#' @section Usage:
#' ```
#' tuner = TunerGenSA$new(ff, terminator, ...)
#' ```
#' See [Tuner] for a description of the interface.
#'
#' @section Arguments:
#' * `ff` ([FitnessFunction]):
#'   Black box function used for the tuning.
#' * `terminator` ([Terminator]):
#'   Terminator to control the termination.
#'   Will be triggered by the hooks of the [FitnessFunction].
#' * `...`:
#'   Additional arguments passed to [GenSA::GenSA].
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerGenSA].
#' The interface is described in [Tuner].
#'
#' @name TunerGenSA
#' @keywords internal
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' resampling$param_set$values$folds = 2
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' task$measures = measures
#' param_set = paradox::ParamSet$new(
#'   params = list(
#'     paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#'   )
#' )
#' ff = FitnessFunction$new(task, learner, resampling, param_set)
#'
#' terminator = TerminatorEvaluations$new(10)
#' rs = TunerGenSA$new(ff, terminator)
#' rs$tune()$tune_result()
NULL

#' @export
#' @include Tuner.R
TunerGenSA = R6Class("TunerGenSA",
  inherit = Tuner,
  public = list(
    initialize = function(ff, terminator, ...) {
      if (any(ff$param_set$storage_type != "numeric")) {
        err_msg = "Parameter types needs to be numeric"
        lg$error(err_msg)
        stopf(err_msg)
      }

      # Default settings:
      settings = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      super$initialize(id = "GenSA", ff = ff, terminator = terminator, settings = insert_named(settings, list(...)))
    }
  ),
  private = list(
    tune_step = function() {
      objective = function(x, ff) {

        param_value = lapply(x, function(param) param)

        # x sometimes comes without names, set them manually:
        names(param_value) = self$ff$param_set$ids()
        dt_param_value = do.call(data.table::data.table, param_value)

        private$eval_design_terminator(paradox::Design$new(ff$param_set, dt_param_value, remove_dupl = TRUE))

        # Get estimated generalization error. Use the negation if the measures needs to be minimized:
        performance = unlist(ff$bmr$data[.N]$performance)[[1]]
        if (!ff$task$measures[[1]]$minimize) {
          return(-performance)
        }
        return(performance)
      }
      nuisance = GenSA::GenSA(fn = objective, lower = self$ff$param_set$lower, upper = self$ff$param_set$upper,
        control = self$settings, ff = self$ff)
    }
  )
)
