#' @title Extract Inner Tuning Results
#' 
#' @description 
#' Extract inner tuning results of nested resampling. Implemented for
#' [mlr3::ResampleResult] and [mlr3::BenchmarkResult]. The function iterates
#' over the [AutoTuner] objects and binds the tuning results to a
#' [data.table::data.table()]. [AutoTuner] must be initialized with
#' `store_tuning_instance = TRUE` and `resample()` or `benchmark()` must be
#' called with `store_models = TRUE`. The resampling `iteration` is added to the
#' table and for [mlr3::BenchmarkResult], the number of the `experiment` is
#' added.
#' 
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult])\cr
#'  Must contain an [AutoTuner].
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' learner$param_set$values$cp = to_tune(0.001, 0.1)
#' 
#' at = AutoTuner$new(
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 5),
#'   tuner = tnr("grid_search"),
#'   store_tuning_instance = TRUE)
#' 
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(task, at, resampling_outer, store_models = TRUE)
#' 
#' extract_inner_tuning_results(rr)
extract_inner_tuning_results = function (x) {
   UseMethod("extract_inner_tuning_results", x)
}

#' @export
extract_inner_tuning_results.ResampleResult = function(x) {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model)) {
    stopf("Set `store_models = TRUE` in `resample()` or `benchmark()`.")
  }
  if (is.null(rr$learners[[1]]$model$tuning_instance)) {
    stopf("Set `store_tuning_instance = TRUE` in %s.", format(rr$learners[[1]]))
  }
  imap_dtr(rr$learners, function(learner, i) {
    assert_r6(learner, "AutoTuner")
    data = learner$tuning_result
    set(data, j = "iteration", value = i)
  })
}

#' @export
extract_inner_tuning_results.BenchmarkResult = function(x) {
  bmr = assert_benchmark_result(x)
  imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_results(rr)
     set(data, j = "experiment", value = i)
  }, .fill = TRUE)
  # reorder dt
  cols_x = map_chr(bmr$resample_results$resample_result, function(rr) rr$learners[[1]]$archive$cols_x)
  cols_y = map_chr(bmr$resample_results$resample_result, function(rr) rr$learners[[1]]$archive$cols_y)
  setcolorder(tab, unique(c(cols_x, cols_y)))
  tab
}

#' @title Extract Inner Tuning Archives
#' 
#' @description 
#' Extract inner tuning archives of nested resampling. Implemented for
#' [mlr3::ResampleResult] and [mlr3::BenchmarkResult]. The function iterates
#' over the [AutoTuner] objects and binds the tuning archives to a
#' [data.table::data.table()]. [AutoTuner] must be initialized with
#' `store_tuning_instance = TRUE` and `resample()` or `benchmark()` must be
#' called with `store_models = TRUE`. The resampling `iteration` is added to the
#' table and for [mlr3::BenchmarkResult], the number of the `experiment` is
#' added.
#' 
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult])\cr
#'  Must contain an [AutoTuner].
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' learner$param_set$values$cp = to_tune(0.001, 0.1)
#' 
#' at = AutoTuner$new(
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 5),
#'   tuner = tnr("grid_search"),
#'   store_tuning_instance = TRUE)
#' 
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(task, at, resampling_outer, store_models = TRUE)
#' 
#' extract_inner_tuning_archives(rr)
extract_inner_tuning_archives = function (x) {
   UseMethod("extract_inner_tuning_archives", x)
}

#' @export
extract_inner_tuning_archives.ResampleResult = function(x) {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model)) {
    stopf("Set `store_models = TRUE` in `resample()` or `benchmark()`.")
  }
  if (is.null(rr$learners[[1]]$model$tuning_instance)) {
    stopf("Set `store_tuning_instance = TRUE` in %s.", format(rr$learners[[1]]))
  }
  imap_dtr(rr$learners, function(learner, i) {
    assert_r6(learner, "AutoTuner")
    data = learner$archive$data
    set(data, j = "iteration", value = i)
  })
}

#' @export
extract_inner_tuning_archives.BenchmarkResult = function(x) {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_archives(rr)
     set(data, j = "experiment", value = i)
  }, .fill = TRUE)
  # reorder dt
  cols_x = map_chr(bmr$resample_results$resample_result, function(rr) rr$learners[[1]]$archive$cols_x)
  cols_y = map_chr(bmr$resample_results$resample_result, function(rr) rr$learners[[1]]$archive$cols_y)
  setcolorder(tab, unique(c(cols_x, cols_y)))
  tab
}