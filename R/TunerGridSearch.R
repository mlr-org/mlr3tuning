#' @title TunerGridSearch
#'
#' @name mlr_tuners_grid_search
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per parameter,
#' see [paradox::generate_design_grid()].
#' The points of the grid are evaluated in a random order.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @section Construction:
#' ```
#' TunerGridSearch$new()
#' tnr("grid_search")
#' ```
#'
#' @section Parameters:
#' * `resolution` :: `integer(1)`\cr
#'   Resolution of the grid, see [paradox::generate_design_grid()].
#' * `param_resolutions` :: named `integer()` \cr
#'   Resolution per parameter, named by parameter ID, see [paradox::generate_design_grid()].
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(
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
        properties = "dependencies"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      pv = self$param_set$values
      g = generate_design_grid(instance$param_set, resolution = pv$resolution, param_resolutions = pv$param_resolutions)
      ch = chunk_vector(seq_row(g$data), chunk_size = pv$batch_size, shuffle = TRUE)
      for (inds in ch) {
        instance$eval_batch(g$data[inds])
      }
    }
  )
)

mlr_tuners$add("grid_search", TunerGridSearch)
