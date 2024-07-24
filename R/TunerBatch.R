#' @title Class for Batch Tuning Algorithms
#'
#' @include mlr_tuners.R
#'
#' @description
#' The [TunerBatch] implements the optimization algorithm.
#'
#' @details
#' [TunerBatch] is an abstract base class that implements the base functionality each tuner must provide.
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner.
#'  * Specify the private abstract method `$.optimize()` and use it to call into your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstanceBatchSingleCrit]/[TuningInstanceBatchMultiCrit] object `instance`, so each batch is possibly executed in parallel via [mlr3::benchmark()], and all evaluations are stored inside of `instance$archive`.
#'  * Before the batch evaluation, the [bbotk::Terminator] is checked, and if it is positive, an exception of class `"terminated_error"` is generated.
#'    In the  later case the current batch of evaluations is still stored in `instance`, but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best configuration from `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [bbotk::Terminator] may be evaluated, as the Terminator is only checked before a batch evaluation, and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide yourself how to estimate the final configuration in the instance and its estimated performance.
#'    The default behavior is: We pick the best resample-experiment, regarding the given measure, then assign its configuration and aggregated performance to the instance.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify tuning of your subclass.
#'   See details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final configuration is selected.
#'   See details sections.
#'
#' @inheritSection Tuner Resources
#'
#' @template param_id
#' @template param_param_set
#' @template param_param_classes
#' @template param_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @export
TunerBatch = R6Class("TunerBatch",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "tuner_batch",
      param_set,
      param_classes,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
      ) {
      super$initialize(
        id = id,
        param_set = param_set,
        param_classes = param_classes,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceBatchSingleCrit] or [TuningInstanceBatchMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveBatchTuning] that resides in the [TuningInstanceBatchSingleCrit]/[TuningInstanceBatchMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      assert_tuning_instance_batch(inst)
      result = optimize_batch_default(inst, self)
      inst$objective$.__enclos_env__$private$.xss = NULL
      inst$objective$.__enclos_env__$private$.design = NULL
      inst$objective$.__enclos_env__$private$.benchmark_result = NULL
      inst$objective$.__enclos_env__$private$.aggregated_performance = NULL
      return(result)
    }
  )
)

