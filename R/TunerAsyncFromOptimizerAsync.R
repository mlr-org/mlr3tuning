#' @title TunerAsyncFromOptimizerAsync
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [Tuner].
#'
#' @template param_man
#'
#' @keywords internal
#' @export
TunerAsyncFromOptimizerAsync = R6Class("TunerAsyncFromOptimizerAsync",
    inherit = TunerAsync,
    public = list(

      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param optimizer [bbotk::Optimizer]\cr
      #' Optimizer that is called.
      initialize = function(optimizer, man = NA_character_) {
        private$.optimizer = assert_r6(optimizer, "OptimizerAsync")
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
      #' Performs the tuning on a [TuningInstanceSingleCrit] /
      #' [TuningInstanceMultiCrit] until termination. The single evaluations and
      #' the final results will be written into the [ArchiveTuning] that
      #' resides in the [TuningInstanceSingleCrit]/[TuningInstanceMultiCrit].
      #' The final result is returned.
      #'
      #' @param inst ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]).
      #'
      #' @return [data.table::data.table].
      optimize = function(inst) {
        assert_multi_class(inst, c("TuningInstanceAsyncSingleCrit", "TuningInstanceAsyncMultiCrit"))

        # evaluate learner with default hyperparameter values
        if (get_private(inst)$.evaluate_default) {
          xdt = default_configuration(inst)
          inst$archive$push_tasks(transpose_list(xdt))
        }

        res = private$.optimizer$optimize(inst)
        if (!inst$objective$keep_hotstart_stack) inst$objective$hotstart_stack = NULL
        res
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
