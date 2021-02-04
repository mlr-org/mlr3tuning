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

#' @title Retrieve Inner Tuning Results
#' 
#' @description 
#' Retrieves innner tuning results of nested resampling. The function iterates
#' over the [AutoTuner]s stored in the [mlr3::ResampleResult] and binds the
#' tuning results to a [data.table::data.table]. [AutoTuner] must be
#' initialized with `store_tuning_instance = TRUE`.
#' 
#' @param resample_result ([mlr3::ResampleResult])\cr
#' Must contain an [AutoTuner].
#' @return [data.table::data.table].
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
#' inner_tuning_results(rr)
inner_tuning_results = function(resample_result) {
  rr = assert_resample_result(resample_result)
  map(rr$learners, function(learner) assert_r6(learner, "AutoTuner"))

  rbindlist(map(rr$learners, function(learner) learner$tuning_result))
}
