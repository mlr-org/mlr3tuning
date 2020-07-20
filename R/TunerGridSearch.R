#' @title TunerGridSearch
#'
#' @name mlr_tuners_grid_search
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per
#' parameter, see [paradox::generate_design_grid()]. The points of the grid are
#' evaluated in a random order.
#'
#'
#' @templateVar id grid_search
#' @template section_dictionary_tuners
#' @template section_parallelization
#' @template section_logging
#'
#'
#' @inheritSection bbotk::OptimizerGridSearch Parameters
#'
#' @export
#' @template example
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerGridSearch$new()
      )
    }
  )
)

mlr_tuners$add("grid_search", TunerGridSearch)
