
#' @title Multi Criteria Tuning Instance for Batch Tuning
#'
#' @description
#' `TuningInstanceMultiCrit` is a deprecated class that is now a wrapper around [TuningInstanceBatchMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#'
#' @export
TuningInstanceMultiCrit = R6Class("TuningInstanceMultiCrit",
  inherit = TuningInstanceBatchMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      terminator,
      search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL
      ) {

      message("TuningInstanceMultiCrit is deprecated. Use TuningInstanceBatchMultiCrit instead.")

      super$initialize(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        terminator = terminator,
        search_space = search_space,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks)
    }
  )
)
