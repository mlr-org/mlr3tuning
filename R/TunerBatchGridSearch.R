#' @title Hyperparameter Tuning with Grid Search
#'
#' @name mlr_tuners_grid_search
#'
#' @description
#' Subclass for grid search tuning.
#'
#' @details
#' The grid is constructed as a Cartesian product over discretized values per parameter, see [paradox::generate_design_grid()].
#' If the learner supports hotstarting, the grid is sorted by the hotstart parameter (see also [mlr3::HotstartStack]).
#' If not, the points of the grid are evaluated in a  random order.
#'
#' @templateVar id grid_search
#' @template section_dictionary_tuners
#'
#' @section Control Parameters:
#' \describe{
#' \item{`resolution`}{`integer(1)`\cr
#'   Resolution of the grid, see [paradox::generate_design_grid()].}
#' \item{`param_resolutions`}{named `integer()`\cr
#'   Resolution per parameter, named by parameter ID, see [paradox::generate_design_grid()].}
#' \item{`batch_size`}{`integer(1)`\cr
#'   Maximum number of points to try in a batch.}
#' }
#'
#' @inheritSection Tuner Resources
#' @template section_progress_bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerBatchGridSearch
#' @template section_optimizer
#'
#' @family Tuner
#' @export
#' @template example
TunerBatchGridSearch = R6Class("TunerBatchGridSearch",
  inherit = TunerBatchFromOptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerBatchGridSearch$new(),
        man = "mlr3tuning::mlr_tuners_grid_search"
      )
    }
  )
)

mlr_tuners$add("grid_search", TunerBatchGridSearch)
