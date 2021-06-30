#' @title Function for Tuning
#' 
#' @description
#' Function to tune a [mlr3::Learner].
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
#' @return `TuningInstanceSingleCrit` | `TuningInstanceMultiCrit`
#'  
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_search_space
#' @template param_store_models
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
tune = function(method, task, learner, resampling, measures, term_evals = NULL, term_time = NULL, search_space = NULL,
  store_models = FALSE, ...) {
  assert_choice(method, mlr_tuners$keys())
  tuner = tnr(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  if (!is.list(measures)) {
    instance = TuningInstanceSingleCrit$new(task, learner, resampling, measures, terminator, search_space, 
      store_models = store_models)
  } else {
    instance = TuningInstanceMultiCrit$new(task, learner, resampling, measures, terminator, search_space, 
      store_models = store_models)
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