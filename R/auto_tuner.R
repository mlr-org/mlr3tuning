#' @title Function for Automatic Tuning
#'
#' @inherit AutoTuner description
#' @inheritSection TuningInstanceSingleCrit Default Measures
#' @inheritSection AutoTuner Resources
#' @inherit AutoTuner details
#' @inheritSection AutoTuner Nested Resampling
#'
#' @return [AutoTuner].
#'
#' @template param_tuner
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_term_evals
#' @template param_term_time
#' @template param_terminator
#' @template param_search_space
#' @template param_store_tuning_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#' @template param_method
#'
#' @export
#' @examples
#' at = auto_tuner(
#'   tuner = tnr("random_search"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' at$train(tsk("pima"))
auto_tuner = function(tuner, learner, resampling, measure = NULL, term_evals = NULL, term_time = NULL, terminator = NULL, search_space = NULL, store_tuning_instance = TRUE, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, evaluate_default = FALSE, callbacks = list(), method) {
  if (!missing(method)) {
    message("The `method` argument is deprecated and will be removed in a future release. Please use `tuner` instead.")
    tuner = method
  }


  terminator = terminator %??% terminator_selection(term_evals, term_time)

  AutoTuner$new(
    tuner = tuner,
    learner = learner,
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    search_space = search_space,
    store_tuning_instance = store_tuning_instance,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    allow_hotstart = allow_hotstart,
    keep_hotstart_stack = keep_hotstart_stack,
    evaluate_default = evaluate_default,
    callbacks = callbacks)
}
