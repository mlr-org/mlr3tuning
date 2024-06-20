#' @title Hyperparameter Tuning with Internal Tuning
#'
#' @include Tuner.R
#' @name mlr_tuners_internal
#'
#' @description
#' Subclass to conduct only internal hyperparameter tuning for a [mlr3::Learner].
#'
#' @note
#' The selected [mlr3::Measure] does not influence the tuning result.
#' To change the loss-function for the internal tuning, consult the hyperparameter documentation of the tuned [mlr3::Learner].
#'
#' @templateVar id internal
#' @template section_dictionary_tuners
#'
#' @inheritSection Tuner Resources
#' @template section_progress_bars
#' @template section_logging
#' @family Tuner
#'
#' @export
#' @examplesIf mlr3misc::require_namespaces("mlr3learners", "xgboost", quietly = TRUE)
#' library(mlr3learners)
#'
#' # Retrieve task
#' task = tsk("pima")
#'
#' # Load learner and set search space
#' learner = lrn("classif.xgboost",
#'   nrounds = to_tune(upper = 1000, internal = TRUE),
#'   early_stopping_rounds = 10,
#'   validate = "test"
#' )
#'
#' # Internal hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   tnr("internal"),
#'   tsk("iris"),
#'   learner,
#'   rsmp("cv", folds = 3),
#'   msr("classif.ce")
#' )
#'
#' # best performing hyperparameter configuration
#' instance$result_learner_param_vals
#'
#' instance$result_learner_param_vals$internal_tuned_values
TunerBatchInternal = R6Class("TunerBatchInternal",
  inherit = TunerBatch,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "internal",
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit"),
        label = "Internal Optimizer",
        man = "mlr3tuning::mlr_tuners_internal"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      inst$eval_batch(data.table())
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

