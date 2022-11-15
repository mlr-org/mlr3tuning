#' @title Hyperparameter Tuning with Random Search
#'
#' @name mlr_tuners_random_search
#' @include Tuner.R
#'
#' @description
#' Subclass for random search tuning.
#'
#' @details
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' @templateVar id random_search
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerRandomSearch Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerRandomSearch Progress Bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerRandomSearch
#' @template section_optimizer
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @template example
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerRandomSearch$new(),
        man = "mlr3tuning::mlr_tuners_random_search"
      )
    }
  )
)

mlr_tuners$add("random_search", TunerRandomSearch)
