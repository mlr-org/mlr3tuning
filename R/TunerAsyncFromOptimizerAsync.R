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
        private$.optimizer = assert_optimizer_async(optimizer)
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
      #' the final results will be written into the [ArchiveAsyncTuning] that
      #' resides in the [TuningInstanceBatchSingleCrit]/[TuningInstanceBatchMultiCrit].
      #' The final result is returned.
      #'
      #' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]).
      #'
      #' @return [data.table::data.table].
      optimize = function(inst) {
        assert_tuning_instance_async(inst)
        if (!inst$search_space$length && !is.null(inst$internal_search_space)) {
          stopf("To only conduct internal parameter tuning, use tnr('internal')")
        }
        private$.optimizer$optimize(inst)
      }
    ),

    active = list(

      #' @field param_set ([paradox::ParamSet])\cr
      #' Set of control parameters.
      param_set = function(rhs) {
        if (!missing(rhs) && !identical(rhs, private$.optimizer$param_set)) {
          stop("$param_set is read-only.")
        }
        private$.optimizer$param_set
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
