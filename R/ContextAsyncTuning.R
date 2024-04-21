#' @title Evaluation Context
#'
#' @description
#' The [ContextAsyncTuning] allows [CallbackTuning]s to access and modify data while a hyperparameter configuration is evaluated.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_tuning()] for a list of stages that access [ContextAsyncTuning].
#'
#' @details
#' This context is re-created each time a new batch of hyperparameter configurations is evaluated.
#' Changes to `$objective_tuning`, `$resample_result` are discarded after the function is finished.
#' Modification on the data table in `$aggregated_performance` are written to the archive.
#' Any number of columns can be added.
#'
#' @export
ContextAsyncTuning = R6Class("ContextAsyncTuning",
  inherit = ContextAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param instance ([TuningInstanceAsyncSingleCrit] | [TuningInstanceAsyncMultiCrit]).
    #' @param tuner ([TunerAsync]).
    initialize = function(instance, tuner) {
      super$initialize(instance, tuner)
    }
  ),

  active = list(
    #' @field xs (list())\cr
    #'   The hyperparameter configuration currently evaluated.
    #'   Contains the values on the learner scale i.e. transformations are applied.
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
        return(get_private(self$instance$objective)$.resample_result)
      } else {
        self$instance$objective$.__enclos_env__$private$.resample_result = rhs
      }
    },

    #' @field aggregated_performance ([data.table::data.table])\cr
    #'   Aggregated performance scores and training time of the latest batch.
    #'   This data table is passed to the archive.
    #'   A callback can add additional columns which are also written to the archive.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.aggregated_performance)
      } else {
        self$instance$objective$.__enclos_env__$private$.aggregated_performance = rhs
      }
    }
  )
)
