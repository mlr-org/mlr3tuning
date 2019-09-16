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
#' TunerGenSA$new()
#' tnr("gensa")
#' ```
#' @section Parameters:
#' For the meaning of the control param see [GenSA::GenSA()].
#' For consistency, we have removed all GenSA control parameters which refer to the stopping of the algorithm and
#' where mlr3 terminators allow to obtain the same behavior.
#'
#' * `smooth` :: `logical(1)`\cr
#' * `temperature` :: `numeric(1)`\cr
#' * `acceptance.param` :: `numeric(1)`\cr
#' * `verbose` :: `logical(1)`\cr
#' * `trace.mat` :: `logical(1)`\cr
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGenSA = R6Class("TunerGenSA", inherit = Tuner,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamLgl$new("smooth", default = TRUE),
        ParamDbl$new("temperature", default = 5230),
        ParamDbl$new("acceptance.param", default = -5),
        ParamLgl$new("verbose", default = FALSE),
        ParamLgl$new("trace.mat", default = TRUE)
      ))
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = "singlecrit",
        packages = "GenSA"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      v = self$param_set$values
      v$maxit = 1000000 # make sure GenSA does not stop
      GenSA::GenSA(fn = instance$tuner_objective, lower = instance$param_set$lower,
        upper = instance$param_set$upper, control = v)
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
