#' @title Hyperparameter Tuning with Generalized Simulated Annealing
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
#' @inheritSection bbotk::OptimizerGenSA Progress Bars
#' 
#' @template section_parallelization
#' @template section_logging
#'
#' @source
#' `r format_bib("tsallis_1996", "xiang_2013")`
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
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
