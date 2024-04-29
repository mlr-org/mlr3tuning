#' @title Hyperparameter Tuning with Asynchronous Random Search
#'
#' @name mlr_tuners_async_random_search
#'
#' @description
#' Subclass for asynchronous random search tuning.
#'
#' @details
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' @templateVar id async_random_search
#' @template section_dictionary_tuners
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @family TunerAsync
#' @export
TunerAsyncRandomSearch = R6Class("TunerAsyncRandomSearch",
  inherit = TunerAsyncFromOptimizerAsync,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = bbotk::OptimizerAsyncRandomSearch$new(),
       man = "mlr3tuning::mlr_tuners_async_random_search"
     )
   }
  )
)

mlr_tuners$add("async_random_search", TunerAsyncRandomSearch)
