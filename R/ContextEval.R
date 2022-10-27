#' @title Evaluation Context
#'
#' @description
#' The [ContextEval] allows [CallbackTuning]s to access and modify data while a batch of hyperparameter configurations is evaluated.
#' See section on active bindings for a list of modifiable objects.
#' See [callback_tuning()] for a list of stages which access [ContextEval].
#'
#' @details
#' This context is re-created each time a new batch of hyperparameter configurations is evaluated.
#' Changes to `$objective_tuning`, `$design` `$benchmark_result` are discarded after the function is finished.
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
      self$objective_tuning = assert_r6(objective_tuning, "ObjectiveTuning")
    }
  ),

  active = list(
    #' @field xss (list())\cr
    #'   The hyperparameter configurations of the latest batch.
    #'   Contains the values on the learner scale i.e. transformations are applied.
    #'   See `$xdt` in [bbotk::ContextOptimization] for the untransformed values.
    xss = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.xss)
      } else {
        get_private(self$objective_tuning)$.xss = rhs
      }
    },

    #' @field design ([data.table::data.table])\cr
    #'   The benchmark design of the latest batch.
    design = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.design)
      } else {
        get_private(self$objective_tuning)$.design = rhs
      }
    },

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #'   The benchmark result of the latest batch.
    benchmark_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.benchmark_result)
      } else {
        get_private(self$objective_tuning)$.benchmark_result = rhs
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
        get_private(self$objective_tuning)$.aggregated_performance = rhs
      }
    }
  )
)
