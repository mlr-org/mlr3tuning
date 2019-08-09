#' @title TunerRandomSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for random search tuning.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Number of `batch_size` points are evaluated per iteration (potentially in parallel),
#' then the termination criteria are checked.
#'
#' @section Construction:
#' ```
#' tuner = TunerRandomSearch$new(batch_size = 1L)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'   Stored in `settings`.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(
    initialize = function(batch_size = 1L) {
      batch_size = assert_count(batch_size, coerce = TRUE)
      s = list(batch_size = batch_size)
      super$initialize(settings = s)
    }
  ),

  private = list(
    tune_internal = function() {
      while (TRUE) {  # iterate until we have an exception from eval_batch
        design = generate_design_random(self$pe$param_set, self$settings$batch_size)
        self$pe$eval_batch(design$data)
      }
    }
  )
)
