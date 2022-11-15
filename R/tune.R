#' @title Function for Tuning a Learner
#'
#' @include TuningInstanceSingleCrit.R ArchiveTuning.R
#'
#' @description
#' Function to tune a [mlr3::Learner].
#' The function internally creates a [TuningInstanceSingleCrit] or [TuningInstanceMultiCrit] which describe the tuning problem.
#' It executes the tuning with the [Tuner] (`method`) and returns the result with the tuning instance (`$result`).
#' The [ArchiveTuning] (`$archive`) stores all evaluated hyperparameter configurations and performance scores.
#'
#' @details
#' The [mlr3::Task], [mlr3::Learner], [mlr3::Resampling], [mlr3::Measure] and [Terminator] are used to construct a [TuningInstanceSingleCrit].
#' If multiple performance [Measures][Measure] are supplied, a [TuningInstanceMultiCrit] is created.
#' The parameter `term_evals` and `term_time` are shortcuts to create a [Terminator].
#' If both parameters are passed, a [TerminatorCombo] is constructed.
#' For other [Terminators][Terminator], pass one with `terminator`.
#' If no termination criterion is needed, set `term_evals`, `term_time` and `terminator` to `NULL`.
#' The search space is created from [paradox::TuneToken] or is supplied by `search_space`.
#'
#' @inheritSection TuningInstanceSingleCrit Resources
#' @inheritSection ArchiveTuning Analysis
#'
#' @param method (`character(1)` | [Tuner])\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary or [Tuner] object.
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [TuningInstanceSingleCrit] and multiple measures a [TuningInstanceMultiCrit].
#'   If `NULL`, default measure is used.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#'
#' @return [TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]
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
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#'
#' @export
#' @examples
#' # Hyperparameter optimization on the Palmer Penguins data set
#' task = tsk("pima")
#'
#' # Load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE)
#' )
#'
#' # Run tuning
#' instance = tune(
#'   method = tnr("random_search", batch_size = 2),
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Set optimal hyperparameter configuration to learner
#' learner$param_set$values = instance$result_learner_param_vals
#'
#' # Train the learner on the full data set
#' learner$train(task)
#'
#' # Inspect all evaluated configurations
#' as.data.table(instance$archive)
tune = function(method, task, learner, resampling, measures = NULL, term_evals = NULL, term_time = NULL, terminator = NULL, search_space = NULL, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, evaluate_default = FALSE, callbacks = list(), ...) {
  tuner = if (is.character(method)) {
    assert_choice(method, mlr_tuners$keys())
    tnr(method, ...)
  } else {
    assert_tuner(method)
  }
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  TuningInstance = if (!is.list(measures)) TuningInstanceSingleCrit else TuningInstanceMultiCrit
  instance = TuningInstance$new(task, learner, resampling, measures, terminator, search_space, store_benchmark_result, store_models, check_values, allow_hotstart, keep_hotstart_stack, evaluate_default, callbacks)

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
