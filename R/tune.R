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
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
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
tune = function(method, task, learner, resampling, measures = NULL, term_evals = NULL, term_time = NULL,
  search_space = NULL, store_models = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE,
  evaluate_default = FALSE, ...) {
  assert_choice(method, mlr_tuners$keys())
  tuner = tnr(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  TuningInstance = if (!is.list(measures)) TuningInstanceSingleCrit else TuningInstanceMultiCrit
  instance = TuningInstance$new(task, learner, resampling, measures, terminator, search_space,
      store_models = store_models, allow_hotstart = allow_hotstart, keep_hotstart_stack = keep_hotstart_stack)

  if (evaluate_default) evaluate_default_values(instance)

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

evaluate_default_values = function(inst) {
  # get hyperparameter defaults
  # values are on the learner scale i.e. possible transformation are already applied
  xss = default_values(inst$objective$learner, inst$search_space, inst$objective$task)

  # parameters with exp transformation and log inverse transformation
  has_logscale = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_logscale)
  # parameters with unknown inverse transformation
  has_trafo = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_trafo)
  # parameter set with trafo
  has_extra_trafo = get_private(inst$search_space)$.has_extra_trafo

  if (any(has_trafo) || has_extra_trafo) {
    stop("Cannot evaluate default hyperparameter values. Search space contains transformation functions with unknown inverse function.")
  }

  # inverse parameter with exp transformation
  xdt = as.data.table(map_if(xss, has_logscale, log))

  # eval default hyperparameter values
  inst$eval_batch(xdt)
}
