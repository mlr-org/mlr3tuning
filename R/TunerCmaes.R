#' @title Hyperparameter Tuning with Covariance Matrix Adaptation Evolution Strategy
#'
#' @name mlr_tuners_cmaes
#'
#' @description
#' Subclass that implements CMA-ES calling [adagio::pureCMAES()]
#' from package \CRANpkg{adagio}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_tuners
#' @template section_logging
#'
#' @inheritSection bbotk::OptimizerCmaes Parameters
#' @inheritSection bbotk::OptimizerCmaes Progress Bars
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @template example
TunerCmaes = R6Class("TunerCmaes",
  inherit = TunerFromOptimizer,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = OptimizerCmaes$new()
     )
   }
  )
)

mlr_tuners$add("cmaes", TunerCmaes)
