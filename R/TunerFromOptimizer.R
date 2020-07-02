#' @title TunerFromOptimizer
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [Tuner]
#' @export
TunerFromOptimizer = R6Class("TunerFromOptimizer",
    inherit = Tuner,
    public = list(

      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param optimizer [bbotk::Optimizer]\cr
      #' Optimizer that is called.
      initialize = function(optimizer) {
        private$.optimizer = assert_optimizer(optimizer)
        super$initialize(param_set = optimizer$param_set,
                         param_classes = optimizer$param_classes,
                         properties = optimizer$properties)
      },

      #' @description
      #' Performs the tuning on a [TuningInstanceSingleCrit] /
      #' [TuningInstanceMultiCrit] until termination.
      #'
      #' @param inst ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]).
      #'
      #' @return Modified `self`.
      optimize = function(inst) {
        # We check for both classes since there is no TuningInstance super
        # class anymore and OptimInstance would not ensure that we are in the
        # scope of mlr3tuning
        assert_multi_class(inst, c("TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
        private$.optimizer$optimize(inst)
      }
    ),

    private = list(
      .optimizer = NULL
    )
)