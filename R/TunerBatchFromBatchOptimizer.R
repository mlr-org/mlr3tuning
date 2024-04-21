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
      #' Performs the tuning on a [TuningInstanceBatchSingleCrit] /
      #' [TuningInstanceBatchMultiCrit] until termination. The single evaluations and
      #' the final results will be written into the [ArchiveTuning] that
      #' resides in the [TuningInstanceBatchSingleCrit]/[TuningInstanceBatchMultiCrit].
      #' The final result is returned.
      #'
      #' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]).
      #'
      #' @return [data.table::data.table].
      optimize = function(inst) {
        assert_multi_class(inst, c("TuningInstanceBatchSingleCrit", "TuningInstanceBatchMultiCrit"))

        # start optimization
        start_optimize_batch_bbotk(inst, self)

        # evaluate learner with default hyperparameter values
        if (get_private(inst)$.evaluate_default) evaluate_default(inst)

        # run optimization
        run_optimize_batch_bbotk(inst, private$.optimizer)

        # remove hotstart stack if not needed
        if (!inst$objective$keep_hotstart_stack) inst$objective$hotstart_stack = NULL

        # finish optimization
        finish_optimize_batch_bbotk(inst)
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
