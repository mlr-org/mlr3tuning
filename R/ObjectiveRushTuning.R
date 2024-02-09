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
#' @template param_hotstart_threshold
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

    #' @field hotstart_stack ([mlr3::HotstartStack]).
    hotstart_stack = NULL,

    #' @field allow_hotstart (`logical(1)`).
    allow_hotstart = NULL,

    #' @field keep_hotstart_stack (`logical(1)`).
    keep_hotstart_stack = NULL,

    #' @field callbacks (List of [CallbackTuning]s).
    callbacks = NULL,

    #' @field default_values (named `list`).
    default_values = NULL,

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
      allow_hotstart = FALSE,
      hotstart_threshold = NULL,
      callbacks = list()) {

      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$learner$param_set$assert_values = FALSE
      self$default_values = self$learner$param_set$values
      self$measures = assert_measures(as_measures(measures), task = self$task, learner = self$learner)
      self$resampling = resampling
      self$store_models = assert_flag(store_models)
      self$store_benchmark_result = assert_flag(store_benchmark_result)
      self$allow_hotstart = assert_flag(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      if (self$allow_hotstart) {
        if (!is.null(hotstart_threshold)) {
          hotstart_threshold = set_names(assert_numeric(hotstart_threshold), self$learner$param_set$ids(tags = "hotstart"))
        }
        self$hotstart_stack = HotstartStack$new(hotstart_threshold = hotstart_threshold)
      }
      self$keep_hotstart_stack = FALSE
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(
        id = sprintf("%s_on_%s", self$learner$id, self$task$id),
        properties = "noisy",
        domain = self$learner$param_set,
        codomain = measures_to_codomain(self$measures),
        constants = ps(),
        check_values = check_values)
    }
  ),

  private = list(
    .eval = function(xs) {
      context = ContextEval$new(self)

      lg$debug("Evaluating hyperparameter configuration %s", as_short_string(xs))

      # combining default values and hyperparameter configuration avoids cloning
      private$.xs = insert_named(self$default_values, xs)
      call_back("on_eval_after_xs", self$callbacks, context)
      self$learner$param_set$set_values(.values = private$.xs, .insert = FALSE)

      if (self$allow_hotstart) {
        lg$debug("Adding hotstart stack to learner.")
        self$learner$hotstart_stack = self$hotstart_stack
      }

      private$.resample_result = resample(self$task, self$learner, self$resampling, store_models = TRUE, allow_hotstart = self$allow_hotstart, clone = character(0))
      call_back("on_eval_after_resample", self$callbacks, context)

      private$.aggregated_performance = as.list(private$.resample_result$aggregate(self$measures))

      lg$debug("Aggregated performance %s", as_short_string(private$.aggregated_performance))

      runtime_learners = extract_runtime(private$.resample_result)
      private$.aggregated_performance = c(private$.aggregated_performance, list(runtime_learners = runtime_learners))
      if (self$allow_hotstart) self$hotstart_stack$add(private$.resample_result$learners)
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
