#' @title TunerGridSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per parameter,
#' see [paradox::generate_design_grid].
#' The grid is searched in random order.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Number of `batch_size` points are evaluated per iteration (potentially in parallel),
#' then the termination criteria are checked.
#'
#' @section Construction:
#' ```
#' tuner = TunerGridSearch$new(pe, terminator, resolution = 10L, batch_size = 1L)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `resolution` :: `integer(1)`\cr
#'   Resolution of the grid, see [paradox::generate_design_grid].
#'   Stored in `settings`.
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
    initialize = function(pe, terminator = NULL, resolution = 10L, batch_size = 1L) {
      resolution = assert_int(resolution, lower = 1L, coerce = TRUE)
      batch_size = assert_int(batch_size, lower = 1L, coerce = TRUE)
      s = list(batch_size = batch_size, resolution = resolution)
      super$initialize(pe = pe, terminator = terminator, settings = s)
      return(self)
    }
  ),

  private = list(
    tune_internal = function() {
      g = generate_design_grid(self$pe$param_set, resolution = self$settings$resolution)
      ch = chunk_vector(1:nrow(g$data), chunk_size = self$settings$batch_size, shuffle = TRUE)
      for (i in 1:length(ch)) {
        self$eval_batch(g$data[ch[[i]],])
      }
    }
  )
)
