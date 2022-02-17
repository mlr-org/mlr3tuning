#' @title Function for Nested Resampling
#'
#' @description
#' Function to conduct nested resampling.
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary.
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#'
#' @return [mlr3::ResampleResult]
#'
#' @template param_task
#' @template param_learner
#' @template param_measure
#' @template param_search_space
#'
#' @export
#' @examples
#' rr = tune_nested(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2),
#'   measure = msr("classif.ce"),
#'   term_evals = 2,
#'   batch_size = 2)
#'
#' # retrieve inner tuning results.
#' extract_inner_tuning_results(rr)
#'
#' # performance scores estimated on the outer resampling
#' rr$score()
#'
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
tune_nested = function(method, task, learner, inner_resampling, outer_resampling, measure = NULL, term_evals = NULL,
  term_time = NULL, search_space = NULL, ...) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)

  at = auto_tuner(method = method, learner = learner, resampling = inner_resampling, measure = measure,
    term_evals = term_evals, term_time = term_time, search_space = search_space, ...)
  resample(task, at, outer_resampling, store_models = TRUE)
}
