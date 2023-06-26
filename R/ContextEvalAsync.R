#' @title Async Evaluation Context
#'
#' @description
#' The [ContextEvalAsync] allows [CallbackTuning]s to access and modify data while a batch of hyperparameter configurations is evaluated.
#' See section on active bindings for a list of modifiable objects.
#' See [callback_tuning()] for a list of stages which access [ContextEval].
#'
#' @details
#' This context is re-created each time a new hyperparameter configuration is evaluated.
#' Changes to `$objective_tuning`, `$design` `$benchmark_result` are discarded after the function is finished.
#' Modification on the list in `$res` are written to the archive.
#' Any number of elements can be added.
#'
#' @export
ContextEvalAsync = R6Class("ContextEvalAsync",
  inherit = mlr3misc::Context,
  public = list(

    #' @field objective_tuning [ObjectiveTuning].
    objective_tuning = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the new callback.
    #' @param objective_tuning [ObjectiveTuning].
    initialize = function(objective_tuning) {
      self$objective_tuning = assert_r6(objective_tuning, "ObjectiveTuning")
    }
  ),

  active = list(
    #' @field xs (list())\cr
    #'   The hyperparameter configurations of the evaluation.
    #'   Contains the values on the learner scale i.e. transformations are applied.
    #'   See `$xdt` in [bbotk::ContextOptimization] for the untransformed values.
    xs = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.xs)
      } else {
        get_private(self$objective_tuning)$.xs = rhs
      }
    },

    #' @field resample_result ([mlr3::ResampleResult])\cr
    #'   The resample result of the evaluation.
    resample_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.resample_result)
      } else {
        get_private(self$objective_tuning)$.resample_result = rhs
      }
    },

    #' @field res (`list`)\cr
    #'   Aggregated performance scores and training time of the evaluation.
    #'   This list is passed to the archive.
    #'   A callback can add additional elements which are also written to the archive.
    res = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.res)
      } else {
        get_private(self$objective_tuning)$.res = rhs
      }
    }
  )
)
