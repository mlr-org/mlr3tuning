#' @title TunerRandomSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Tuner child class to conduct a random search.
#'
#' @section Construction:
#' ```
#' tuner = TunerRandomSearch$new(pe, terminator, batch_size = 100L)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(
    initialize = function(pe, terminator, batch_size = 1L) {
      batch_size = assert_count(batch_size, coerce = TRUE)
      super$initialize(id = "random_search", pe = pe, terminator = terminator, settings = list(batch_size = batch_size))
    }
  ),

  private = list(
    tune_step = function() {
      tryCatch({
        while (TRUE) {  # iterate until we have an exception from eval_batch
          design = generate_design_random(self$pe$param_set, self$settings$batch_size)
          self$eval_batch(design$data)
        }
      }, terminated_message = function(w){})
    }
  )
)
