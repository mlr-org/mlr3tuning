#' @title Syntactic Sugar for Tuning Objects Construction
#'
#' @description
#' Functions to retrieve objects, set parameters and assign to fields in one go.
#' Relies on [mlr3misc::dictionary_sugar_get()] to extract objects from the respective [mlr3misc::Dictionary]:
#'
#' * `tnr()` for a [Tuner] from [mlr_tuners].
#' * `tnrs()` for a list of [Tuners][Tuner] from [mlr_tuners].
#' * `trm()` for a [Terminator] from [mlr_terminators].
#' * `trms()` for a list of [Terminators][Terminator] from [mlr_terminators].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [R6::R6Class] object of the respective type, or a list of [R6::R6Class] objects for the plural versions.
#'
#' @export
#' @examples
#' # random search tuner with batch size of 5
#' tnr("random_search", batch_size = 5)
#'
#' # run time terminator with 20 seconds
#' trm("run_time", secs = 20)
tnr = function(.key, ...) {
  dictionary_sugar_get(mlr_tuners, .key, ...)
}


#' @rdname tnr
#' @export
tnrs = function(.keys, ...) {
  dictionary_sugar_mget(mlr_tuners, .keys, ...)
}

#' @title Syntactic Sugar for Tuning Instance Construction
#'
#' @description
#' Function to construct a [TuningInstanceBatchSingleCrit] or [TuningInstanceBatchMultiCrit].
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [TuningInstanceBatchSingleCrit] and multiple measures a [TuningInstanceBatchMultiCrit].
#'   If `NULL`, default measure is used.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_hotstart_threshold
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#'
#' @inheritSection TuningInstanceBatchSingleCrit Resources
#' @inheritSection TuningInstanceBatchSingleCrit Default Measures
#'
#' @export
#' @inherit TuningInstanceBatchSingleCrit examples
ti = function(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  search_space = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  allow_hotstart = FALSE,
  hotstart_threshold = NULL,
  keep_hotstart_stack = FALSE,
  evaluate_default = FALSE,
  callbacks = list()
  ) {
  measures = assert_measures(as_measures(measures))
  TuningInstance = if (length(measures) > 1) TuningInstanceBatchMultiCrit else TuningInstanceBatchSingleCrit

  TuningInstance$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures,
    terminator = terminator,
    search_space = search_space,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    allow_hotstart = allow_hotstart,
    hotstart_threshold = hotstart_threshold,
    keep_hotstart_stack = keep_hotstart_stack,
    evaluate_default = evaluate_default,
    callbacks = callbacks)
}

#' @title Syntactic Sugar for Asynchronous Tuning Instance Construction
#'
#' @description
#' Function to construct a [TuningInstanceAsyncSingleCrit] or [TuningInstanceAsyncMultiCrit].
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [TuningInstanceSingleAsyncCrit] and multiple measures a [TuningInstanceAsyncMultiCrit].
#'   If `NULL`, default measure is used.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_hotstart_threshold
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#' @template param_rush
#'
#' @inheritSection TuningInstanceBatchSingleCrit Resources
#' @inheritSection TuningInstanceBatchSingleCrit Default Measures
#'
#' @export
#' @inherit TuningInstanceBatchSingleCrit examples
ti_async = function(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  search_space = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  evaluate_default = FALSE,
  callbacks = list(),
  rush = NULL
  ) {
  measures = assert_measures(as_measures(measures))
  TuningInstance = if (length(measures) > 1) TuningInstanceAsyncMultiCrit else TuningInstanceAsyncSingleCrit

  TuningInstance$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures,
    terminator = terminator,
    search_space = search_space,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    evaluate_default = evaluate_default,
    callbacks = callbacks,
    rush = rush)
}
