#' @title Hyperparameter Tuning with Generalized Simulated Annealing
#'
#' @name mlr_tuners_gensa
#'
#' @description
#' Subclass for generalized simulated annealing tuning.
#' Calls [GenSA::GenSA()] from package \CRANpkg{GenSA}.
#'
#' @details
#' In contrast to the [GenSA::GenSA()] defaults, we set `smooth = FALSE` as a default.
#'
#' @templateVar id gensa
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerGenSA Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerGenSA Progress Bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerGenSA
#' @template section_optimizer
#'
#' @source
#' `r format_bib("tsallis_1996", "xiang_2013")`
#'
#' @family Tuner
#' @export
#' @template example
TunerGenSA = R6Class("TunerGenSA",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      optimizer = OptimizerGenSA$new()
      optimizer$param_set$values$smooth = FALSE
      super$initialize(
        optimizer = optimizer,
        man = "mlr3tuning::mlr_tuners_gensa"
      )
    }
  )
)

mlr_tuners$add("gensa", TunerGenSA)
