#' @title Syntactic Sugar for Automatic Tuning
#'
#' @description
#' Function to create an [AutoTuner] object.
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#'
#' @return [AutoTuner]
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_search_space
#' @template param_store_models
#'
#' @export
#' @examples
#' at = auto_tuner(
#'   method = "random_search",
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' at$train(tsk("pima"))
auto_tuner = function(method, learner, resampling, measure = NULL, term_evals = NULL, term_time = NULL, search_space = NULL,
  store_models = FALSE, ...) {
  assert_choice(method, mlr_tuners$keys())
  tuner = tnr(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  AutoTuner$new(learner = learner, resampling = resampling, measure = measure, terminator = terminator, tuner = tuner,
    search_space = search_space, store_models = store_models)
}
