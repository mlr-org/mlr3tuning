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

    #' @field callbacks (List of [mlr3misc::Callback]).
    callbacks = NULL,

    #' @field default_values (named `list()`).
    default_values = NULL,

    #' @field internal_search_space ([paradox::ParamSet]).
    #' Internal search space for internal tuning.
    internal_search_space = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      internal_search_space = NULL
      ) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$learner$param_set$assert_values = FALSE
      self$measures = assert_measures(as_measures(measures), task = self$task, learner = self$learner)
      self$store_models = assert_flag(store_models)
      self$store_benchmark_result = assert_flag(store_benchmark_result) || self$store_models
      self$callbacks = assert_callbacks(as_callbacks(callbacks))
      self$internal_search_space = if (!is.null(internal_search_space)) assert_param_set(internal_search_space)

      self$default_values = self$learner$param_set$values

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
