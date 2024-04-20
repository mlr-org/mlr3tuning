#' @title Evaluation Context
#'
#' @description
#' The [ContextEval] allows [CallbackTuning]s to access and modify data while a hyperparameter configuration is evaluated.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_tuning()] for a list of stages that access [ContextEval].
#'
#' @details
#' This context is re-created each time a new batch of hyperparameter configurations is evaluated.
#' Changes to `$objective_tuning`, `$resample_result` are discarded after the function is finished.
#' Modification on the data table in `$aggregated_performance` are written to the archive.
#' Any number of columns can be added.
#'
#' @export
ContextEval = R6Class("ContextEval",
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
      self$objective_tuning = assert_r6(objective_tuning, "ObjectiveTuningAsync")
    }
  ),

  active = list(
    #' @field xs (list())\cr
    #'   The hyperparameter configuration currently evaluated.
    #'   Contains the values on the learner scale i.e. transformations are applied.
    #'   See `$xdt` in [bbotk::ContextOptimization] for the untransformed values.
    xs = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.xs)
      } else {
        self$objective_tuning$.__enclos_env__$private$.xs = rhs
      }
    },

    #' @field resample_result ([mlr3::BenchmarkResult])\cr
    #'   The resample result of the hyperparameter configuration currently evaluated.
    resample_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.resample_result)
      } else {
        self$objective_tuning$.__enclos_env__$private$.resample_result = rhs
      }
    },

    #' @field aggregated_performance ([data.table::data.table])\cr
    #'   Aggregated performance scores and training time of the latest batch.
    #'   This data table is passed to the archive.
    #'   A callback can add additional columns which are also written to the archive.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.aggregated_performance)
      } else {
        self$objective_tuning$.__enclos_env__$private$.aggregated_performance = rhs
      }
    }
  )
)
