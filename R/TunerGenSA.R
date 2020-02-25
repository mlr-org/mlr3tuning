#' @title TunerGenSA
#'
#' @name mlr_tuners_gensa
#' @include Tuner.R
#'
#' @description
#' Subclass for generalized simulated annealing tuning calling [GenSA::GenSA()] from package \CRANpkg{GenSA}.
#'
#' @section Parameters:
#' * `smooth` (`logical(1)`).
#' * `temperature` (`numeric(1)`).
#' * `acceptance.param` (`numeric(1)`).
#' * `verbose` (`logical(1)`).
#' * `trace.mat` (`logical(1)`).
#'
#' For the meaning of the control parameters, see [GenSA::GenSA()].
#' Note that we have removed all control parameters which refer to the termination of the algorithm and
#' where our terminators allow to obtain the same behavior.
#'
#' @templateVar id gensa
#' @template section_dictionary_tuners
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGenSA = R6Class("TunerGenSA", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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
    .tune = function(instance) {
      v = self$param_set$values
      v$maxit = .Machine$integer.max # make sure GenSA does not stop
      GenSA::GenSA(fn = instance$tuner_objective, lower = instance$param_set$lower,
        upper = instance$param_set$upper, control = v)
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
