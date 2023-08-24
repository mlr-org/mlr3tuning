#' @title Class for Tuning Objective
#'
#' @description
#' Stores the objective function that estimates the performance of hyperparameter configurations.
#' This class is usually constructed internally by the [TuningInstanceSingleCrit] or [TuningInstanceMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_callbacks
#'
#' @export
ObjectiveTuning = R6Class("ObjectiveTuning",
  inherit = Objective,
  public = list(

    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

     #' @field resampling ([mlr3::Resampling]).
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure]).
    measures = NULL,

    #' @field store_models (`logical(1)`).
    store_models = NULL,

    #' @field store_benchmark_result (`logical(1)`).
    store_benchmark_result = NULL,

    #' @field archive ([ArchiveTuning]).
    archive = NULL,

    #' @field hotstart_stack ([mlr3::HotstartStack]).
    hotstart_stack = NULL,

    #' @field allow_hotstart (`logical(1)`).
    allow_hotstart = NULL,

    #' @field keep_hotstart_stack (`logical(1)`).
    keep_hotstart_stack = NULL,

    #' @field callbacks (List of [CallbackTuning]s).
    callbacks = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([ArchiveTuning])\cr
    #'   Reference to archive of [TuningInstanceSingleCrit] | [TuningInstanceMultiCrit].
    #'   If `NULL` (default), benchmark result and models cannot be stored.
    initialize = function(task, learner, resampling, measures, store_benchmark_result = TRUE, store_models = FALSE, check_values = TRUE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, archive = NULL, callbacks = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)

      self$store_benchmark_result = assert_flag(store_benchmark_result)
      self$allow_hotstart = assert_flag(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      if (self$allow_hotstart) self$hotstart_stack = HotstartStack$new()
      self$keep_hotstart_stack = assert_flag(keep_hotstart_stack)
      self$store_models = assert_flag(store_models)
      self$archive = assert_r6(archive, "ArchiveTuning", null.ok = TRUE)
      if (is.null(self$archive)) self$allow_hotstart = self$store_benchmark_result = self$store_models = FALSE
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id), properties = "noisy",
        domain = self$learner$param_set, codomain = measures_to_codomain(self$measures),
        constants = ps(resampling = p_uty()), check_values = check_values)

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  ),

  private = list(
    .eval = function(xs, ...) {
      #context = ContextEvalAsync$new(self)
      private$.xs = xs

      learner = self$learner$clone(deep = TRUE)
      learner$param_set$set_values(.values = private$.xs)
      if (self$allow_hotstart) learner$hotstart_stack = self$hotstart_stack
      #call_back("on_eval_after_design", self$callbacks, context)

      private$.resample_result = resample(self$task, learner, self$resampling, store_models = self$store_models, allow_hotstart = self$allow_hotstart, clone = character())
      #call_back("on_eval_after_benchmark", self$callbacks, context)

      aggregated_performance = as.list(private$.resample_result$aggregate(self$measures))
      runtime_learners = sum(map_dbl(get_private(private$.resample_result)$.data$learner_states(get_private(private$.resample_result)$.view), function(state) state$train_time + state$predict_time))
      private$.res = c(aggregated_performance, list(runtime_learners = runtime_learners))

      if (self$allow_hotstart) self$hotstart_stack$add(private$.resample_result$learners)
      if (!self$store_models) private$.resample_result$discard(models = TRUE)
      if (self$store_benchmark_result) private$.res = c(private$.res, list(resample_result = list(private$.resample_result)))

      call_back("on_eval_before_archive", self$callbacks, context)
      private$.res
    },

    .xs = NULL,
    .design = NULL,
    .resample_result = NULL,
    .aggregated_performance = NULL,
    .res = NULL
  )
)
