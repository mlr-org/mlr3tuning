#' @title Function for Tuning
#'
#' @description
#' Function to tune a [mlr3::Learner].
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
#' @return `TuningInstanceSingleCrit` | `TuningInstanceMultiCrit`
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_search_space
#' @template param_store_models
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#'
#' @export
#' @examples
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' instance = tune(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # apply hyperparameter values to learner
#' learner$param_set$values = instance$result_learner_param_vals
tune = function(method, task, learner, resampling, measures = NULL, term_evals = NULL, term_time = NULL, search_space = NULL, store_models = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, ...) {
  tuner = if (is.character(method)) {
    assert_choice(method, mlr_tuners$keys())
    tnr(method, ...)
  } else {
    assert_tuner(method)
  }
  terminator = terminator_selection(term_evals, term_time)

  if (!is.list(measures)) {
    instance = TuningInstanceSingleCrit$new(task = task, learner = learner, resampling = resampling,
      measure = measures, terminator = terminator, search_space = search_space, store_models = store_models,
      allow_hotstart = allow_hotstart, keep_hotstart_stack = keep_hotstart_stack)
  } else {
    instance = TuningInstanceMultiCrit$new(task = task, learner = learner, resampling = resampling, measures = measures,
      terminator = terminator, search_space = search_space, store_models = store_models,
      allow_hotstart = allow_hotstart, keep_hotstart_stack = keep_hotstart_stack)
  }

  tuner$optimize(instance)
  instance
}

terminator_selection = function(term_evals, term_time) {
  assert_int(term_evals, null.ok = TRUE)
  assert_int(term_time, null.ok = TRUE)

  if (is.null(term_evals) && is.null(term_time)) {
    trm("none")
  } else if (!is.null(term_evals) && !is.null(term_time)) {
    trm("combo", list(trm("evals", n_evals = term_evals), trm("run_time", secs = term_time)))
  } else if (!is.null(term_evals)) {
    trm("evals", n_evals = term_evals)
  } else if (!is.null(term_time)) {
    trm("run_time", secs = term_time)
  }
}
