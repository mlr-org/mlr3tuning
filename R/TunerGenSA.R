#' @title TunerGenSA
#'
#' @name mlr_tuners_gensa
#'
#' @description
#' Subclass for generalized simulated annealing tuning calling [GenSA::GenSA()]
#' from package \CRANpkg{GenSA}.
#'
#' @templateVar id gensa
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`smooth`}{`logical(1)`}
#' \item{`temperature`}{`numeric(1)`}
#' \item{`acceptance.param`}{`numeric(1)`}
#' \item{`verbose`}{`logical(1)`}
#' \item{`trace.mat`}{`logical(1)`}
#' }
#'
#' For the meaning of the control parameters, see [GenSA::GenSA()]. Note that we
#' have removed all control parameters which refer to the termination of the
#' algorithm and where our terminators allow to obtain the same behavior.
#'
#' @export
#' @template example
TunerGenSA = R6Class("TunerGenSA",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerGenSA$new()
      )
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
