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
#' tuner = TunerGenSA$new(...)
#' ```
#' For arguments, see [Tuner], and additionally:
#' * `...`\cr
#'   Settings passed down do [GenSA::GenSA()]
#'   Default settings are `smooth = FALSE`, `acceptance.param = -15`,
#'   `simple.function = FALSE`, and `temperature = 250`.
#'   Stored in `settings`.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGenSA = R6Class("TunerGenSA",
  inherit = Tuner,
  public = list(
    initialize = function(...) {
      # Default settings:
      s = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      s = insert_named(s, list(...))
      super$initialize(
        param_classes = c("ParamDbl"),
        settings = s
      )
    }
  ),
  private = list(
    tune_internal = function(pe) {
      objective = function(x, pe) {
        measure = pe$measures[[1L]]

        n = length(pe$bmr$hashes)
        params = setDT(as.list(x))
        setnames(params, pe$param_set$ids())
        pe$eval_batch(params)

        perf = pe$bmr$resample_result(n + 1)$aggregate(measure)
        if (measure$minimize) perf else -perf
      }
      GenSA::GenSA(fn = objective, lower = pe$param_set$lower, upper = pe$param_set$upper,
        control = self$settings, pe = pe)
    }
  )
)
