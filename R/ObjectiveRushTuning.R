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
ObjectiveRushTuning = R6Class("ObjectiveRushTuning",
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
    initialize = function(task, learner, resampling, measures, store_benchmark_result = TRUE, store_models = FALSE, check_values = TRUE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, callbacks = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      learner$param_set$assert_values = FALSE
      self$measures = assert_measures(as_measures(measures), task = self$task, learner = self$learner)
      self$resampling = resampling
      self$store_models = assert_flag(store_models)


      self$store_benchmark_result = assert_flag(store_benchmark_result)
      self$allow_hotstart = assert_flag(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      if (self$allow_hotstart) self$hotstart_stack = HotstartStack$new()
      self$keep_hotstart_stack = assert_flag(keep_hotstart_stack)
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id), properties = "noisy",
        domain = self$learner$param_set, codomain = measures_to_codomain(self$measures),
        constants = ps(), check_values = check_values)
    }
  ),

  private = list(
    .eval = function(xs) {

      self$learner$param_set$set_values(.values = xs)
      if (self$allow_hotstart) self$learner$hotstart_stack = self$hotstart_stack

      resample_result = resample(self$task, self$learner, self$resampling, store_models = self$store_models || self$allow_hotstart, allow_hotstart = self$allow_hotstart, clone = character(0))
      aggregated_performance = as.list(resample_result$aggregate(self$measures))

      runtime_learners = sum(map_dbl(get_private(resample_result)$.data$learner_states(get_private(resample_result)$.view), function(state) state$train_time + state$predict_time))
      extra = list(runtime_learners = runtime_learners)

      if (self$allow_hotstart) self$hotstart_stack$add(resample_result$learners)
      if (!self$store_models) resample_result$discard(models = TRUE)
      if (self$store_benchmark_result) extra = c(extra, list(resample_result = list(resample_result)))

      c(aggregated_performance, extra)
    }
  )
)
