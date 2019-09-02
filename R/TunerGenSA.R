#' @title TunerGenSA
#'
#' @aliases mlr_tuners_gensa
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
#'
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
TunerGenSA = R6Class("TunerGenSA", inherit = Tuner,

  public = list(
    initialize = function(...) {
      # Default settings:
      s = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      s = insert_named(s, list(...))
      super$initialize(
        param_classes = "ParamDbl",
        settings = s,
        packages = "GenSA"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      GenSA::GenSA(fn = instance$tuner_objective, lower = instance$param_set$lower,
        upper = instance$param_set$upper, control = self$settings)
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
