#' @title Hyperparameter Tuning with Asynchronous Grid Search
#'
#' @name mlr_tuners_async_grid_search
#'
#' @description
#' Subclass for asynchronous grid search tuning.
#'
#' @templateVar id async_design_points
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerAsyncGridSearch Parameters
#'
#' @family TunerAsync
#' @export
TunerAsyncGridSearch = R6Class("TunerAsyncGridSearch",
  inherit = TunerAsyncFromOptimizerAsync,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = bbotk::OptimizerAsyncGridSearch$new(),
       man = "mlr3tuning::mlr_tuners_async_grid_search"
     )
   }
  )
)

mlr_tuners$add("async_grid_search", TunerAsyncGridSearch)
