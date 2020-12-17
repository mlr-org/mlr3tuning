#' @title TunerMbo
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' Tune hyperparameters using Bayesian Optimization.
#' This is a minimal interface internally passing on to [mlr3mbo::OptimizerMbo].
#' For additional information and documentation see [mlr3mbo::OptimizerMbo].
#'
#' @export
TunerMbo = R6Class("TunerMbo",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param ... Parameters passed to [mlr3mbo::OptimizerMbo].
    initialize = function(...) {
      require_namespaces("mlr3mbo")
      super$initialize(optimizer = mlr3mbo::OptimizerMbo$new(...))
    }
  )
)
