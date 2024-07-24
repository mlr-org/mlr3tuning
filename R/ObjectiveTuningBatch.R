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
#' @template param_internal_search_space
#'
#' @export
ObjectiveTuningBatch = R6Class("ObjectiveTuningBatch",
  inherit = ObjectiveTuning,
  public = list(

    #' @field archive ([ArchiveBatchTuning]).
    archive = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([ArchiveBatchTuning])\cr
    #'   Reference to archive of [TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit].
    #'   If `NULL` (default), benchmark result and models cannot be stored.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      archive = NULL,
      callbacks = NULL,
      internal_search_space = NULL
      ) {
      self$archive = assert_r6(archive, "ArchiveBatchTuning", null.ok = TRUE)
      if (is.null(self$archive)) store_benchmark_result = store_models = FALSE

      super$initialize(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks,
        internal_search_space = internal_search_space
      )
    }
  ),

  private = list(
    .eval_many = function(xss, resampling) {
      private$.xss = xss

     private$.design = if (length(resampling) > 1) {
        param_values = map(xss, function(xs) list(xs))
        data.table(task = list(self$task), learner = list(self$learner), resampling = resampling, param_values = param_values)
      } else {
        benchmark_grid(self$task, self$learner, resampling, param_values = list(xss))
      }

      call_back("on_eval_after_design", self$callbacks, self$context)

      # learner is already cloned, task and resampling are not changed
      private$.benchmark_result = benchmark(
        design = private$.design,
        store_models = self$store_models,
        clone = character(0))
      call_back("on_eval_after_benchmark", self$callbacks, self$context)

      # aggregate performance scores
      private$.aggregated_performance = private$.benchmark_result$aggregate(self$measures, conditions = TRUE)[, c(self$codomain$target_ids, "warnings", "errors"), with = FALSE]

      # add runtime to evaluations
      time = map_dbl(private$.benchmark_result$resample_results$resample_result, function(rr) {
        extract_runtime(rr)
      })

      set(private$.aggregated_performance, j = "runtime_learners", value = time)

      # add internal tuned values
      if (!is.null(self$internal_search_space)) {
        internal_tuned_values = map(private$.benchmark_result$resample_results$resample_result, function(resample_result) {
          extract_inner_tuned_values(resample_result, self$internal_search_space)
        })

        set(private$.aggregated_performance, j = "internal_tuned_values", value = list(internal_tuned_values))
      }

      call_back("on_eval_before_archive", self$callbacks, self$context)

      # store benchmark result in archive
      if (self$store_benchmark_result) {
        self$archive$benchmark_result$combine(private$.benchmark_result)
        set(private$.aggregated_performance, j = "uhash", value = private$.benchmark_result$uhashes)
      }

      # learner is not cloned anymore
      # restore default values
      self$learner$param_set$set_values(.values = self$default_values, .insert = FALSE)

      private$.aggregated_performance
    },

    .xss = NULL,
    .design = NULL,
    .benchmark_result = NULL,
    .aggregated_performance = NULL
  )
)
