#' @title Extract Inner Tuning Archives
#'
#' @description
#' Extract inner tuning archives of nested resampling.
#' Implemented for [mlr3::ResampleResult] and [mlr3::BenchmarkResult].
#' The function iterates over the [AutoTuner] objects and binds the tuning archives to a [data.table::data.table()].
#' [AutoTuner] must be initialized with `store_tuning_instance = TRUE` and [mlr3::resample()] or [mlr3::benchmark()] must be called with `store_models = TRUE`.
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
#' * `runtime_learners` (`numeric(1)`)\cr
#'     Sum of training and predict times logged in learners per [mlr3::ResampleResult] / evaluation.
#'     This does not include potential overhead time.
#' * `timestamp` (`POSIXct`)\cr
#'     Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'     Hyperparameters are evaluated in batches.
#'     Each batch has a unique batch number.
#' * `x_domain` (`list()`)\cr
#'     List of transformed hyperparameter values.
#'     By default this column is unnested.
#' * `x_domain_*` (`any`)\cr
#'     Separate column for each transformed hyperparameter.
#' * `resample_result` ([mlr3::ResampleResult])\cr
#'     Resample result of the inner resampling.
#' * `task_id` (`character(1)`).
#' * `learner_id` (`character(1)`).
#' * `resampling_id` (`character(1)`).
#'
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @param unnest (`character()`)\cr
#'   Transforms list columns to separate columns.
#'   By default, `x_domain` is unnested.
#'   Set to `NULL` if no column should be unnested.
#' @param exclude_columns (`character()`)\cr
#'   Exclude columns from result table.
#'   Set to `NULL` if no column should be excluded.
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
#' # extract inner archives
#' extract_inner_tuning_archives(rr)
extract_inner_tuning_archives = function (x, unnest = "x_domain", exclude_columns = "uhash") {
   UseMethod("extract_inner_tuning_archives")
}

#' @export
extract_inner_tuning_archives.ResampleResult = function(x, unnest = "x_domain", exclude_columns = "uhash") {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model$tuning_instance)) {
    return(data.table())
  }
  tab = imap_dtr(rr$learners, function(learner, i) {
    data = as.data.table(learner$archive, unnest = unnest, exclude_columns = exclude_columns)
    set(data, j = "iteration", value = i)
  })
  tab[, "task_id" := rr$task$id]
  tab[, "learner_id" := rr$learner$id]
  tab[, "resampling_id" := rr$resampling$id]
  cols_x = rr$learners[[1]]$archive$cols_x
  cols_y = rr$learners[[1]]$archive$cols_y
  setcolorder(tab, c("iteration", cols_x, if (!is.null(tab$internal_tuned_values)) "internal_tuned_values", cols_y))
  tab
}

#' @export
extract_inner_tuning_archives.BenchmarkResult = function(x, unnest = "x_domain", exclude_columns = "uhash") {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_archives(rr, unnest = unnest, exclude_columns = exclude_columns)
     if (nrow(data) > 0) set(data, j = "experiment", value = i)
  }, .fill = TRUE)

  if (nrow(tab) > 0) {
    # reorder dt
    cols_x = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_x)))
    cols_y = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_y)))
    cols_x_domain =  if ("x_domain" %in% unnest) names(tab)[grepl("^x_domain_.*", names(tab))] else NULL
    setcolorder(tab, c("experiment", "iteration", unique(c(cols_x, cols_y)), if(!is.null(tab$internal_tuned_values)) "internal_tuned_values", cols_x_domain))
  }
  tab
}
