terminated_error = function(instance) {
  msg = sprintf(
    fmt = "TuningInstance (tsk:%s, lrn:%s, term:%s) terminated",
    instance$objective$task$id,
    instance$objective$learner$id,
    format(instance$terminator)
  )

  set_class(list(message = msg, call = NULL), c(
    "terminated_error", "error", "condition"))
}

#' @title Extract Inner Tuning Results
#' 
#' @description 
#' Extract inner tuning results of nested resampling. Implemented for
#' [mlr3::ResampleResult] and [mlr3::BenchmarkResult]. The function iterates
#' over the [AutoTuner] objects and binds the tuning results to a
#' [data.table::data.table()]. [AutoTuner] must be initialized with
#' `store_tuning_instance = TRUE`. For [mlr3::BenchmarkResult], the number of
#' the `experiment` is added to the table.
#' 
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult])\cr
#'  Must contain an [AutoTuner].
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' task = tsk("iris")
#' search_space = ParamSet$new(
#'   params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1))
#' )
#'
#' at = AutoTuner$new(
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 5),
#'   tuner = tnr("grid_search"),
#'   search_space = search_space,
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
  map_dtr(rr$learners, function(learner) {
    assert_r6(learner, "AutoTuner")
    learner$tuning_result
  })
}

#' @export
extract_inner_tuning_results.BenchmarkResult = function(x) {
  bmr = assert_benchmark_result(x)
  imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_tuning_results(rr)
     set(data, j = "experiment", value = i)
  })
}
