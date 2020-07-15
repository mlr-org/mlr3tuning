#' @title TunerRandomSearch
#'
#' @name mlr_tuners_random_search
#' @include Tuner.R
#'
#' @description
#' Subclass for random search tuning.
#'
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' In order to support general termination criteria and parallelization, we
#' evaluate points in a batch-fashion of size `batch_size`. Larger batches mean
#' we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @templateVar id random_search
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerRandomSearch Parameters
#'
#' @inherit bbotk::OptimizerRandomSearch source
#'
#' @export
#' @template example
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerRandomSearch$new()
      )
    }
  )
)

mlr_tuners$add("random_search", TunerRandomSearch)
