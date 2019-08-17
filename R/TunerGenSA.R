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
    tune_internal = function(inst) {
      objective = function(x, inst) {
        measure = inst$measures[[1L]]

        n = length(inst$bmr$hashes)
        params = setDT(as.list(x))
        setnames(params, inst$param_set$ids())
        inst$eval_batch(params)

        perf = inst$bmr$resample_result(n + 1)$aggregate(measure)
        if (measure$minimize) perf else -perf
      }
      GenSA::GenSA(fn = objective, lower = inst$param_set$lower, upper = inst$param_set$upper,
        control = self$settings, inst = inst)
    }
  )
)
