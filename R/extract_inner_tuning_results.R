#' @title Extract Inner Tuning Results
#'
#' @description
#' Extract inner tuning results of nested resampling.
#' Implemented for [mlr3::ResampleResult] and [mlr3::BenchmarkResult].
#'
#' @details
#' The function iterates over the [AutoTuner] objects and binds the tuning results to a [data.table::data.table()].
#' The [AutoTuner] must be initialized with `store_tuning_instance = TRUE` and [mlr3::resample()] or [mlr3::benchmark()] must be called with `store_models = TRUE`.
#' Optionally, the tuning instance can be added for each iteration.
#'
#' @section Data structure:
#'
#' The returned data table has the following columns:
#'
#' * `experiment` (integer(1))\cr
#'     Index, giving the according row number in the original benchmark grid.
#' * `iteration` (integer(1))\cr
#'     Iteration of the outer resampling.
#' * One column for each hyperparameter of the search spaces.
#' * One column for each performance measure.
#' * `learner_param_vals` (`list()`)\cr
#'     Hyperparameter values used by the learner.
#'     Includes fixed and proposed hyperparameter values.
#' * `x_domain` (`list()`)\cr
#'     List of transformed hyperparameter values.
#' * `tuning_instance` ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit])\cr
#'     Optionally, tuning instances.
#' * `task_id` (`character(1)`).
#' * `learner_id` (`character(1)`).
#' * `resampling_id` (`character(1)`).
#'
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @param tuning_instance (`logical(1)`)\cr
#'   If `TRUE`, tuning instances are added to the table.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' # Nested Resampling on Palmer Penguins Data Set
#'
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' # create auto tuner
#' at = auto_tuner(
#'   tuner = tnr("random_search"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)
#'
#' # extract inner results
#' extract_inner_tuning_results(rr)
extract_inner_tuning_results = function(x, tuning_instance, ...) {
   UseMethod("extract_inner_tuning_results", x)
}

#' @export
#' @rdname extract_inner_tuning_results
extract_inner_tuning_results.ResampleResult = function(x, tuning_instance = FALSE, ...) {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model$tuning_instance)) {
    return(data.table())
  }
  tab = imap_dtr(rr$learners, function(learner, i) {
    data = setalloccol(learner$tuning_result)
    set(data, j = "iteration", value = i)
    if (tuning_instance) set(data, j = "tuning_instance", value = list(learner$tuning_instance))
    data
  })
  tab[, "task_id" := rr$task$id]
  tab[, "learner_id" := rr$learner$id]
  tab[, "resampling_id" := rr$resampling$id]
  cols_x = rr$learners[[1]]$archive$cols_x
  cols_y = rr$learners[[1]]$archive$cols_y
  setcolorder(tab, c("iteration", cols_x, cols_y))
  tab
}

#' @export
#' @rdname extract_inner_tuning_results
extract_inner_tuning_results.BenchmarkResult = function(x, tuning_instance = FALSE, ...) {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_results(rr, tuning_instance = tuning_instance)
     if (nrow(data) > 0) set(data, j = "experiment", value = i)
  }, .fill = TRUE)
  # reorder dt
  if (nrow(tab) > 0) {
    cols_x = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_x)))
    cols_y = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_y)))
    setcolorder(tab, unique(c("experiment", "iteration", cols_x, cols_y)))
  }
  tab
}
