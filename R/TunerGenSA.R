#' @title TunerGenSA
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Tuner child class to conduct generalized simulated annealing (GenSA) as tuning technique.
#' Internally relies on [GenSA::GenSA()] from package \pkg{GenSA}.
#'
#' @section Construction:
#' ```
#' tuner = TunerGenSA$new(pe, terminator, ...)
#' ```
#' For arguments, see [Tuner], and additionally:
#' * `...` :: named `list()`\cr
#'   Settings passed down do [GenSA::GenSA()]
#'   Default settings are `smooth = FALSE`, `acceptance.param = -15`,
#'   `simple.function = FALSE`, and `temperature = 250`.
#'
#' @section Fields:
#' See [Tuner].
#'
#' @section Methods:
#' See [Tuner].
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
        err_msg = "Parameter types needs to be numeric"
        lg$error(err_msg)
        stopf(err_msg)
      }

      # Default settings:
      settings = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      super$initialize(id = "GenSA", pe = pe, terminator = terminator, settings = insert_named(settings, list(...)))
    }
  ),
  private = list(
    tune_step = function() {
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
