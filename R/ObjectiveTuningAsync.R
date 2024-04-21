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
#' @template param_allow_hotstart
#' @template param_hotstart_threshold
#' @template param_callbacks
#'
#' @export
ObjectiveTuningAsync = R6Class("ObjectiveTuningAsync",
  inherit = ObjectiveTuning,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = TRUE,
      callbacks = NULL
      ) {
      #callbacks = walk(as_callbacks(callbacks), function(callback) assert_r6(callback, " CallbackTuningAsync"))

      super$initialize(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks
      )
    }
  ),

  private = list(
    .eval = function(xs, resampling) {
      context = ContextAsyncTuning$new(self)

      lg$debug("Evaluating hyperparameter configuration %s", as_short_string(xs))

      # combine default values and hyperparameter configuration to avoid cloning
      private$.xs = insert_named(self$default_values, xs)

      # set hyperparameter values
      call_back("on_eval_after_xs", self$callbacks, context)
      self$learner$param_set$set_values(.values = private$.xs, .insert = FALSE)

      # resample hyperparameter configuration
      private$.resample_result = resample(self$task, self$learner, self$resampling, store_models = TRUE, allow_hotstart = TRUE, clone = character(0))
      call_back("on_eval_after_resample", self$callbacks, context)

      # aggregate performance
      private$.aggregated_performance = as.list(private$.resample_result$aggregate(self$measures))
      lg$debug("Aggregated performance %s", as_short_string(private$.aggregated_performance))

      # add errors and warnings
      warnings = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "warning")))
      errors = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "error")))
      log = map(get_private(private$.resample_result)$.data$learner_states(), function(s) s$log)
      private$.aggregated_performance = c(private$.aggregated_performance, list(warnings = warnings, errors = errors, log = list(log)))

      # add runtime
      runtime_learners = extract_runtime(private$.resample_result)
      private$.aggregated_performance = c(private$.aggregated_performance, list(runtime_learners = runtime_learners))

      # add benchmark result and models
      if (!self$store_models) {
        lg$debug("Discarding models.")
        private$.resample_result$discard(models = TRUE)
      }
      if (self$store_benchmark_result) {
        lg$debug("Storing resample result.")
        private$.aggregated_performance = c(private$.aggregated_performance, list(resample_result = list(private$.resample_result)))
      }

      call_back("on_eval_before_archive", self$callbacks, context)
      private$.aggregated_performance
    },

    .xs  = NULL,
    .resample_result = NULL,
    .aggregated_performance = NULL
  )
)
