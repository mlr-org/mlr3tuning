#' @title TunerGridSearch
#'
#' @aliases mlr_tuners_grid_search
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per parameter,
#' see [paradox::generate_design_grid].
#' The points of the grid are evaluated in a random order.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @section Construction:
#' ```
#' tuner = TunerGridSearch$new(resolution = 10L, batch_size = 1L)
#' ```
#'
#' * `resolution` :: `integer(1)`\cr
#'   Resolution of the grid, see [paradox::generate_design_grid()].
#'   Stored in `settings`.
#' * `param_resolutions` :: named `integer()` \cr
#'   Resolution per parameter, named by parameter ID, see [paradox::generate_design_grid()].
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'   Stored in `settings`.
#'
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(
    initialize = function(resolution = 10L, param_resolutions = NULL, batch_size = 1L) {
      s = list(
        batch_size = assert_count(batch_size, positive = TRUE, coerce = TRUE),
        resolution = assert_count(resolution, positive = TRUE, coerce = TRUE),
        param_resolutions = assert_integer(param_resolutions, any.missing = FALSE, lower = 1L, names = "unique", null.ok = TRUE)
      )
      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies",
        settings = s
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      g = generate_design_grid(instance$param_set, resolution = self$settings$resolution, param_resolutions = self$settings$param_resolutions)
      ch = chunk_vector(seq_row(g$data), chunk_size = self$settings$batch_size, shuffle = TRUE)
      for (i in seq_along(ch)) {
        instance$eval_batch(g$data[ch[[i]], ])
      }
    }
  )
)

mlr_tuners$add("grid_search", TunerGridSearch)
