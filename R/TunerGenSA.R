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
#' TunerGenSA$new(...)
#' tnr("gensa")
#' ```
#'
#' @section Parameters:
#' * `control`\cr
#'   Settings passed down do [GenSA::GenSA()]
#'   Default settings are `smooth = FALSE`, `acceptance.param = -15`,
#'   `simple.function = FALSE`, and `temperature = 250`.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGenSA = R6Class("TunerGenSA", inherit = Tuner,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("maxit", lower = 1L),
        ParamDbl$new("threshold.stop"),
        ParamInt$new("nb.stop.improvement", lower = 1L),
        ParamLgl$new("smooth", default = TRUE),
        ParamInt$new("max.call", default = 1e7),
        ParamDbl$new("max.time"),
        ParamDbl$new("temperature"),
        ParamDbl$new("acceptance.param"),
        ParamLgl$new("simple.function", default = FALSE)
      ))
      ps$values = list(smooth = FALSE, acceptance.param = -15, simple.function = FALSE, temperature = 250)
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        packages = "GenSA"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      GenSA::GenSA(fn = instance$tuner_objective, lower = instance$param_set$lower,
        upper = instance$param_set$upper, control = self$param_set$values)
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
