#' @title Batch Tuning Context
#'
#' @description
#' A [CallbackBatchTuning] accesses and modifies data during the optimization via the `ContextBatchTuning`.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_batch_tuning()] for a list of stages that access `ContextBatchTuning`.
#'
#' @template param_inst_batch
#' @template param_tuner
#'
#' @export
ContextBatchTuning = R6Class("ContextBatchTuning",
  inherit = ContextBatch,
  active = list(

    #' @field xss (list())\cr
    #'   The hyperparameter configurations of the latest batch.
    #'   Contains the values on the learner scale i.e. transformations are applied.
    #'   See `$xdt` for the untransformed values.
    xss = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.xss)
      } else {
       self$instance$objective$.__enclos_env__$private$.xss = rhs
      }
    },

    #' @field design ([data.table::data.table])\cr
    #'   The benchmark design of the latest batch.
    design = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.design)
      } else {
        self$instance$objective$.__enclos_env__$private$.design = rhs
      }
    },

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #'   The benchmark result of the latest batch.
    benchmark_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.benchmark_result)
      } else {
        self$instance$objective$.__enclos_env__$private$.benchmark_result = rhs
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
