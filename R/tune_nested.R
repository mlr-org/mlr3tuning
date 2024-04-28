#' @title Function for Nested Resampling
#'
#' @description
#' Function to conduct nested resampling.
#'
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#'
#' @return [mlr3::ResampleResult]
#'
#' @template param_tuner
#' @template param_task
#' @template param_learner
#' @template param_measure
#' @template param_terminator
#' @template param_term_evals
#' @template param_term_time
#' @template param_search_space
#' @template param_store_tuning_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#'
#' @export
#' @examples
#' # Nested resampling on Palmer Penguins data set
#' rr = tune_nested(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = tsk("penguins"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2),
#'   measure = msr("classif.ce"),
#'   term_evals = 2)
#'
#' # Performance scores estimated on the outer resampling
#' rr$score()
#'
#' # Unbiased performance of the final model trained on the full data set
#' rr$aggregate()
tune_nested = function(
  tuner,
  task,
  learner,
  inner_resampling,
  outer_resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  search_space = NULL,
  store_tuning_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL
  ) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)

  at = auto_tuner(
    tuner = tuner,
    learner = learner,
    resampling = inner_resampling,
    measure = measure,
    term_evals = term_evals,
    term_time = term_time,
    terminator = terminator,
    search_space = search_space,
    store_tuning_instance = store_tuning_instance,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    callbacks = callbacks)

  resample(task, at, outer_resampling, store_models = TRUE)
}
