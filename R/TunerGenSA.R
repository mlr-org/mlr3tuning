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
#' @inheritSection bbotk::OptimizerGenSA Parameters
#'
#' @inherit bbotk::OptimizerGenSA source
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
