#' @export
TunerSHA = R6Class("TunerSHA",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = bbotk::OptimizerSHA$new(),
        man = "mlr3hyperband::mlr_tuners_hyperband"
      )
    }
  )
)

mlr_tuners$add("sha", TunerSHA)
