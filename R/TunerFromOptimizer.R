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
      #'
      #' @param param_set [paradox::ParamSet]\cr
      #' Set of control parameters for tuner.
      #'
      #' @param param_classes `character()`\cr
      #' Supported parameter classes for learner hyperparameters that the tuner
      #' can optimize, subclasses of [paradox::Param].
      #'
      #' @param properties `character()`\cr
      #' Set of properties of the tuner. Must be a subset of
      #' [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
      #'
      #' @param packages `character()`\cr
      #' Set of required packages. Note that these packages will be loaded via
      #' [requireNamespace()], and are not attached.
      initialize = function(optimizer, param_set, param_classes, properties,
                            packages = character()) {
        private$.optimizer = optimizer
        super$initialize(param_set = param_set, param_classes = param_classes,
                         properties = properties)
      },

      #' @description
      #' Performs the tuning on a [TuningInstance] until termination.
      #'
      #' @param inst [TuningInstance].
      #'
      #' @return Modified `self`.
      optimize = function(inst) {
        # TuningInstanceMulticrit actually does not inherit from TuningInstance but from OptimInstanceMulticrit
        # in the same way as TuningInstance inherits from OptimInstance. Unfortunately multi-inheritance is not
        # in R6.
        assert_multi_class(inst, c("TuningInstance", "TuningInstanceMulticrit"))
        private$.optimizer$optimize(inst)
      }
    ),

    private = list(
      .optimizer = NULL
    )
)
