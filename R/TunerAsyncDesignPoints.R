#' @title Hyperparameter Tuning with Asynchronous Design Points
#'
#' @name mlr_tuners_async_design_points
#'
#' @description
#' Subclass for asynchronous design points tuning.
#'
#' @templateVar id async_design_points
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerAsyncDesignPoints Parameters
#'
#' @family TunerAsync
#' @export
TunerAsyncDesignPoints = R6Class("TunerAsyncDesignPoints",
  inherit = TunerAsyncFromOptimizerAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsyncDesignPoints$new(),
        man = "mlr3tuning::mlr_tuners_async_design_points"
      )
    }
  )
)

mlr_tuners$add("async_design_points", TunerAsyncDesignPoints)
