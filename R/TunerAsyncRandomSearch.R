#' @title Hyperparameter Tuning with Asynchronous Random Search
#'
#' @name mlr_tuners_async_random_search
#' @include Tuner.R
#'
#' @description
#' Subclass for random search tuning with asynchronous evaluations.
#'
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' @templateVar id random_search
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerAsyncRandomSearch Parameters
#' @inheritSection bbotk::OptimizerAsyncRandomSearch Progress Bars
#'
#' @template section_parallelization
#' @template section_logging
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @template example
TunerAsyncRandomSearch = R6Class("TunerAsyncRandomSearch",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsyncRandomSearch$new()
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
      inst$async = TRUE
      super$optimize(inst)
    }
  )
)

mlr_tuners$add("async_random_search", TunerAsyncRandomSearch)
