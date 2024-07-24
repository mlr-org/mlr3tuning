#' @title Class for Tuning Objective
#'
#' @description
#' Stores the objective function that estimates the performance of hyperparameter configurations.
#' This class is usually constructed internally by the [TuningInstanceBatchSingleCrit] or [TuningInstanceBatchMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_callbacks
#'
#' @export
ObjectiveTuningAsync = R6Class("ObjectiveTuningAsync",
  inherit = ObjectiveTuning,
  private = list(
    .eval = function(xs, resampling) {

      lg$debug("Evaluating hyperparameter configuration %s", as_short_string(xs))

      # combine default values and hyperparameter configuration to avoid cloning
      private$.xs = insert_named(self$default_values, xs)

      # set hyperparameter values
      call_back("on_eval_after_xs", self$callbacks, self$context)
      self$learner$param_set$set_values(.values = private$.xs, .insert = FALSE)

      # resample hyperparameter configuration
      private$.resample_result = resample(self$task, self$learner, self$resampling, store_models = TRUE, allow_hotstart = TRUE, clone = character(0))
      call_back("on_eval_after_resample", self$callbacks, self$context)

      # aggregate performance
      private$.aggregated_performance = as.list(private$.resample_result$aggregate(self$measures))
      lg$debug("Aggregated performance %s", as_short_string(private$.aggregated_performance))

      # add runtime, errors and warnings
      warnings = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "warning")))
      errors = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "error")))
      runtime_learners = extract_runtime(private$.resample_result)

      private$.aggregated_performance = c(private$.aggregated_performance, list(runtime_learners = runtime_learners, warnings = warnings, errors = errors))

      # add internal tuned values
      if (!is.null(self$internal_search_space)) {
        lg$debug("Extracting internal tuned values")
        internal_tuned_values = extract_inner_tuned_values(private$.resample_result, self$internal_search_space)
        private$.aggregated_performance = c(private$.aggregated_performance, list(internal_tuned_values = list(internal_tuned_values)))
      }

      # add benchmark result and models
      if (!self$store_models) {
        lg$debug("Discarding models.")
        private$.resample_result$discard(models = TRUE)
      }
      if (self$store_benchmark_result) {
        lg$debug("Storing resample result.")
        private$.aggregated_performance = c(private$.aggregated_performance, list(resample_result = list(private$.resample_result)))
      }

      call_back("on_eval_before_archive", self$callbacks, self$context)
      private$.aggregated_performance
    },

    .xs  = NULL,
    .resample_result = NULL,
    .aggregated_performance = NULL
  )
)
