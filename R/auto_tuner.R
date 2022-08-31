#' @title Function for Automatic Tuning
#'
#' @inherit AutoTuner description
#' @inheritSection AutoTuner Resources
#' @inherit AutoTuner details
#' @inheritSection AutoTuner Nested Resampling
#'
#' @param method (`character(1)` | [Tuner])\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary or [Tuner] object.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#'
#' @return [AutoTuner].
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_search_space
#' @template param_store_tuning_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#'
#' @export
#' @examples
#' at = auto_tuner(
#'   method = tnr("random_search"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' at$train(tsk("pima"))
auto_tuner = function(method, learner, resampling, measure = NULL, term_evals = NULL, term_time = NULL, terminator = NULL, search_space = NULL, store_tuning_instance = TRUE, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, ...) {
  tuner = if (is.character(method)) {
    assert_choice(method, mlr_tuners$keys())
    tnr(method, ...)
  } else {
    assert_tuner(method)
  }
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  AutoTuner$new(learner = learner, resampling = resampling, measure = measure, terminator = terminator, tuner = tuner, search_space = search_space, store_tuning_instance = store_tuning_instance, store_benchmark_result = store_benchmark_result, store_models = store_models, check_values = check_values)
}
