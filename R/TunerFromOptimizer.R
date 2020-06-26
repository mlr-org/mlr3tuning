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
      #' Performs the tuning on a [TuningInstance] until termination.
      #'
      #' @param inst [TuningInstance].
      #'
      #' @return Modified `self`.
      optimize = function(inst) {
        # TuningInstanceMulticrit actually does not inherit from TuningInstance
        # but from OptimInstanceMulticrit in the same way as TuningInstance
        # inherits from OptimInstance. Unfortunately multi-inheritance is not in
        # R6.
        assert_multi_class(inst, c("TuningInstance", "TuningInstanceMulticrit"))
        private$.optimizer$optimize(inst)
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
