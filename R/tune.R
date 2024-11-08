#' @title Function for Tuning a Learner
#'
#' @include TuningInstanceBatchSingleCrit.R ArchiveBatchTuning.R
#'
#' @description
#' Function to tune a [mlr3::Learner].
#' The function internally creates a [TuningInstanceBatchSingleCrit] or [TuningInstanceBatchMultiCrit] which describes the tuning problem.
#' It executes the tuning with the [Tuner] (`tuner`) and returns the result with the tuning instance (`$result`).
#' The [ArchiveBatchTuning] and [ArchiveAsyncTuning] (`$archive`) stores all evaluated hyperparameter configurations and performance scores.
#'
#' You can find an overview of all tuners on our [website](https://mlr-org.com/tuners.html).
#'
#' @details
#' The [mlr3::Task], [mlr3::Learner], [mlr3::Resampling], [mlr3::Measure] and [bbotk::Terminator] are used to construct a [TuningInstanceBatchSingleCrit].
#' If multiple performance [mlr3::Measure]s are supplied, a [TuningInstanceBatchMultiCrit] is created.
#' The parameter `term_evals` and `term_time` are shortcuts to create a [bbotk::Terminator].
#' If both parameters are passed, a [bbotk::TerminatorCombo] is constructed.
#' For other [Terminators][bbotk::Terminator], pass one with `terminator`.
#' If no termination criterion is needed, set `term_evals`, `term_time` and `terminator` to `NULL`.
#' The search space is created from [paradox::TuneToken] or is supplied by `search_space`.
#'
#' @inheritSection TuningInstanceBatchSingleCrit Default Measures
#' @inheritSection TuningInstanceBatchSingleCrit Resources
#'
#' @inheritSection ArchiveBatchTuning Analysis
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [TuningInstanceBatchSingleCrit] and multiple measures a [TuningInstanceBatchMultiCrit].
#'   If `NULL`, default measure is used.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#'
#' @return [TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]
#'
#' @template param_tuner
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_terminator
#' @template param_term_evals
#' @template param_term_time
#' @template param_search_space
#' @template param_internal_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_rush
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
#'   tuner = tnr("random_search", batch_size = 2),
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
tune = function(
  tuner,
  task,
  learner,
  resampling,
  measures = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  search_space = NULL,
  store_benchmark_result = TRUE,
  internal_search_space = NULL,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  rush = NULL
  ) {
  assert_tuner(tuner)
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  instance =  if (inherits(tuner, "TunerAsync")) {
    TuningInstance = if (is.null(measures) || inherits(measures, "Measure")) TuningInstanceAsyncSingleCrit else TuningInstanceAsyncMultiCrit
    TuningInstance$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      search_space = search_space,
      internal_search_space = internal_search_space,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      rush = rush
      )
  } else {
    TuningInstance = if (is.null(measures) || inherits(measures, "Measure")) TuningInstanceBatchSingleCrit else TuningInstanceBatchMultiCrit
    TuningInstance$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      search_space = search_space,
      internal_search_space = internal_search_space,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks)
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
