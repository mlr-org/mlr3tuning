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
#' @template param_keep_hotstart_stack
#' @template param_callbacks
#'
#' @template field_default_values
#'
#' @export
ObjectiveTuning = R6Class("ObjectiveTuning",
  inherit = Objective,
  public = list(

    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

    default_values = NULL,

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

    #' @field callbacks (List of [CallbackTuning]s).
    callbacks = NULL,

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
      callbacks = NULL
      ) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$learner$param_set$assert_values = FALSE
      self$measures = assert_measures(as_measures(measures), task = self$task, learner = self$learner)
      self$store_models = assert_flag(store_models)
      self$store_benchmark_result = assert_flag(store_benchmark_result) || self$store_models
      self$allow_hotstart = assert_flag(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      if (self$allow_hotstart) {
        if (!is.null(hotstart_threshold)) {
          hotstart_threshold = set_names(assert_numeric(hotstart_threshold), self$learner$param_set$ids(tags = "hotstart"))
        }
        self$hotstart_stack = HotstartStack$new(hotstart_threshold = hotstart_threshold)
      }
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(
        id = sprintf("%s_on_%s", self$learner$id, self$task$id),
        properties = "noisy",
        domain = self$learner$param_set,
        codomain = measures_to_codomain(self$measures),
        constants = ps(resampling = p_uty()),
        check_values = check_values)

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  )
)
