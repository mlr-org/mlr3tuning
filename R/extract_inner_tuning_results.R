#' @title Extract Inner Tuning Results
#'
#' @description
#' Extract inner tuning results of nested resampling. Implemented for
#' [mlr3::ResampleResult] and [mlr3::BenchmarkResult]. The function iterates
#' over the [AutoTuner] objects and binds the tuning results to a
#' [data.table::data.table()]. [AutoTuner] must be initialized with
#' `store_tuning_instance = TRUE` and `resample()` or `benchmark()` must be
#' called with `store_models = TRUE`.
#'
#' @section Data structure:
#'
#' The returned data table has the following columns:
#'
#' * `experiment` (integer(1))\cr
#'   Index, giving the according row number in the original benchmark grid.
#' * `iteration` (integer(1))\cr
#'   Iteration of the outer resampling.
#' * One column for each hyperparameter of the search spaces.
#' * One column for each performance measure.
#' * `learner_param_vals` (`list()`)\cr
#'   Hyperparameter values used by the learner. Includes fixed and proposed
#'   hyperparameter values.
#' * `x_domain` (`list()`)\cr
#'   List of transformed hyperparameter values.
#' * `task_id` (`character(1)`).
#' * `learner_id` (`character(1)`).
#' * `resampling_id` (`character(1)`).
#'
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' at = auto_tuner(
#'   method = "grid_search",
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)
#'
#' extract_inner_tuning_results(rr)
extract_inner_tuning_results = function (x) {
   UseMethod("extract_inner_tuning_results", x)
}

#' @export
extract_inner_tuning_results.ResampleResult = function(x) {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model$tuning_instance)) {
    return(data.table())
  }
  tab = imap_dtr(rr$learners, function(learner, i) {
    data = setalloccol(learner$tuning_result)
    set(data, j = "iteration", value = i)
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
extract_inner_tuning_results.BenchmarkResult = function(x) {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_results(rr)
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
