#' @title Internal Tuner
#' @name mlr_tuners_internal
#' @description
#' Use this tuner to only conduct internal hyperparameter tuning for a [`Learner`].
#' Note that the selected [`Measure`][mlr3::Measure] does not influence the tuning result.
#'
#' To change the loss-function for the internal tuning, consult the hyperparameter documentation of the
#' tuned [`Learner`][mlr3::Learner].
#'
#' @templateVar id internal
#' @template section_dictionary_tuners
#' @section Control Parameters:
#' None.
#' @inheritSection Tuner Resources
#' @template section_progress_bars
#' @template section_logging
#' @family Tuner
#' @export
#' @examplesIf mlr3misc::require_namespaces("XGBoost", quietly = TRUE)
#' learner = lrn("classif.xgboost",
#'   nrounds = to_tune(upper = 1000, internal = TRUE),
#'   early_stopping_rounds = 10,
#'   validate = "test"
#' )
#' ti = tune(
#'   tnr("internal"),
#'   tsk("iris"),
#'   learner,
#'   rsmp("cv", folds = 3),
#'   msr("internal_tuned_values")
#' )
#' ti$result_learner_param_vals
#' ti$result_learner_param_vals$internal_tuned_values
TunerBatchInternal = R6Class("TunerBatchInternal",
  inherit = TunerBatchFromOptimizerBatch,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerBatchInternal$new(),
        man = "mlr3tuning::mlr_tuners_internal"
      )
    }
  )
)

mlr_tuners$add("internal", TunerBatchInternal)

OptimizerBatchInternal = R6Class("OptimizerBatchInternal",
  inherit = bbotk::OptimizerBatch,
  public = list(
    initialize = function() {
      super$initialize(
        id = "internal",
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit"),
        label = "Internal Optimizer",
        man = "bbotk::mlr_optimizers_internal"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      inst$eval_batch(data.table())
    }
  )
)

