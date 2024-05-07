#' @title Asynchronous Tuning Context
#'
#' @description
#' A [CallbackAsyncTuning] accesses and modifies data during the optimization via the `ContextAsyncTuning`.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_async_tuning()] for a list of stages that access `ContextAsyncTuning`.
#'
#' @details
#' Changes to `$instance` and `$optimizer` in the stages executed on the workers are not reflected in the main process.
#'
#' @template param_inst_async
#' @template param_tuner
#'
#' @export
ContextAsyncTuning = R6Class("ContextAsyncTuning",
  inherit = ContextAsync,
  active = list(

    #' @field xs (list())\cr
    #'   The hyperparameter configuration currently evaluated.
    #'   Contains the values on the learner scale i.e. transformations are applied.
    xs = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.xs)
      } else {
        self$instance$objective$.__enclos_env__$private$.xs = rhs
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

    #' @field aggregated_performance (`list()`)\cr
    #'   Aggregated performance scores and training time of the evaluated hyperparameter configuration.
    #'   This list is passed to the archive.
    #'   A callback can add additional elements which are also written to the archive.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.aggregated_performance)
      } else {
        self$instance$objective$.__enclos_env__$private$.aggregated_performance = rhs
      }
    }
  )
)
