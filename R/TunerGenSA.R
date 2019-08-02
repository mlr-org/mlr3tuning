#' @title TunerGenSA
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for generalized simulated annealing tuning with [GenSA::GenSA()].
#'
#' @section Construction:
#' ```
#' tuner = TunerGenSA$new(pe, terminator, ...)
#' ```
#' For arguments, see [Tuner], and additionally:
#' * `...`\cr
#'   Settings passed down do [GenSA::GenSA()]
#'   Default settings are `smooth = FALSE`, `acceptance.param = -15`,
#'   `simple.function = FALSE`, and `temperature = 250`.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGenSA = R6Class("TunerGenSA",
  inherit = Tuner,
  public = list(
    initialize = function(pe, terminator, ...) {
      if (any(pe$param_set$storage_type != "numeric")) {
        stopf("Parameter types need to be numeric!")
      }

      # Default settings:
      settings = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      super$initialize(pe = pe, terminator = terminator, settings = insert_named(settings, list(...)))
    }
  ),
  private = list(
    tune_internal = function() {
      objective = function(x, pe) {
        measure = pe$measures[[1L]]

        hashes = pe$bmr$data$hash
        params = setDT(as.list(x))
        setnames(params, pe$param_set$ids())
        self$eval_batch(params)
        new_hash = setdiff(pe$bmr$data$hash, hashes)

        perf = pe$bmr$resample_result(new_hash)$aggregate(measure)
        if (measure$minimize) perf else -perf
      }
      GenSA::GenSA(fn = objective, lower = self$pe$param_set$lower, upper = self$pe$param_set$upper,
        control = self$settings, pe = self$pe)
    }
  )
)
