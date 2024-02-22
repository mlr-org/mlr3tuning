#' @export
TunerRandomSearchV2 = R6Class("TunerRandomSearchV2",
  inherit = TunerFromOptimizer,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = bbotk::OptimizerRandomSearchV2$new(),
       man = "mlr3tuning::mlr_tuners_random_search_v2"
     )
   }
  )
)

mlr_tuners$add("random_search_v2", TunerRandomSearchV2)
