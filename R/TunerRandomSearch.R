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
#' @templateVar id random_search
#' @template section_dictionary_tuners
#' @template section_parallelization
#' @template section_logging
#'
#' @inheritSection bbotk::OptimizerNLoptr Parameters
#'
#' @source
#' \cite{bbotk}{bergstra_2012}
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
