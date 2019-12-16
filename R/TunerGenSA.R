#' @title TunerGenSA
#'
#' @name mlr_tuners_gensa
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for generalized simulated annealing tuning calling [GenSA::GenSA()] from package \CRANpkg{GenSA}.
#'
#' @section Construction:
#' ```
#' TunerGenSA$new()
#' tnr("gensa")
#' ```
#' @section Parameters:
#' * `smooth` :: `logical(1)`\cr
#' * `temperature` :: `numeric(1)`\cr
#' * `acceptance.param` :: `numeric(1)`\cr
#' * `verbose` :: `logical(1)`\cr
#' * `trace.mat` :: `logical(1)`\cr
#'
#' For the meaning of the control parameters, see [GenSA::GenSA()].
#' Note that we have removed all control parameters which refer to the termination of the algorithm and
#' where our terminators allow to obtain the same behavior.
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
        properties = character(),
        packages = "GenSA"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      v = self$param_set$values
      v$maxit = .Machine$integer.max # make sure GenSA does not stop
      GenSA::GenSA(fn = instance$tuner_objective, lower = instance$param_set$lower,
        upper = instance$param_set$upper, control = v)
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
