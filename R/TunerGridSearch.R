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
#' In order to support general termination criteria and parallelization, we
#' evaluate points in a batch-fashion of size `batch_size`. Larger batches mean
#' we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @templateVar id grid_search
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`resolution`}{`integer(1)`\cr
#' Resolution of the grid, see [paradox::generate_design_grid()].}
#' \item{`param_resolutions`}{named `integer()`\cr
#' Resolution per parameter, named by parameter ID, see
#' [paradox::generate_design_grid()].}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of configurations to try in a batch..}
#' }
#'
#' @export
#' @template example
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("batch_size", lower = 1L, tags = "required"),
        ParamInt$new("resolution", lower = 1L),
        ParamUty$new("param_resolutions")
      ))
      ps$values = list(resolution = 10L, batch_size = 1L)
      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pv = self$param_set$values
      g = generate_design_grid(inst$search_space, resolution = pv$resolution,
        param_resolutions = pv$param_resolutions)
      ch = chunk_vector(seq_row(g$data), chunk_size = pv$batch_size,
        shuffle = TRUE)
      for (inds in ch) {
        inst$eval_batch(g$data[inds])
      }
    }
  )
)

mlr_tuners$add("grid_search", TunerGridSearch)
