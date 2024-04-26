#' @title TunerBatchFromOptimizerBatch
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [Tuner].
#'
#' @template param_man
#'
#' @keywords internal
#' @export
TunerBatchFromOptimizerBatch = R6Class("TunerBatchFromOptimizerBatch",
    inherit = TunerBatch,
    public = list(

      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param optimizer [bbotk::Optimizer]\cr
      #' Optimizer that is called.
      initialize = function(optimizer, man = NA_character_) {
        private$.optimizer = assert_optimizer(optimizer)
        packages = union("mlr3tuning", optimizer$packages)
        assert_string(man, na.ok = TRUE)

        super$initialize(
          id = if ("id" %in% names(optimizer)) optimizer$id else "tuner",
          param_set = optimizer$param_set,
          param_classes = optimizer$param_classes,
          properties = optimizer$properties,
          packages = packages,
          label = optimizer$label,
          man = man
        )
      },

      #' @description
      #' Performs the tuning on a [TuningInstanceBatchSingleCrit] / [TuningInstanceBatchMultiCrit] until termination.
      #' The single evaluations and the final results will be written into the [ArchiveTuningBatch] that resides in the [TuningInstanceBatchSingleCrit]/[TuningInstanceBatchMultiCrit].
      #' The final result is returned.
      #'
      #' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]).
      #'
      #' @return [data.table::data.table].
      optimize = function(inst) {
        assert_tuning_instance_batch(inst)
        private$.optimizer$optimize(inst)
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
