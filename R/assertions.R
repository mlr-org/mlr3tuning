#' @export
assert_tuner = function(tuner) {
  assert_r6(tuner, "Tuner")
}

#' @export
assert_tuners = function(tuners) {
  invisible(lapply(tuners, assert_tuner))
}

#' @export
assert_tuner_async = function(tuner) {
  assert_r6(tuner, "TunerAsync")
}

#' @export
assert_tuner_batch = function(tuner) {
  assert_r6(tuner, "TunerBatch")
}

#' @export
assert_tuning_instance = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceBatchSingleCrit",
    "TuningInstanceBatchMultiCrit",
    "TuningInstanceAsyncSingleCrit",
    "TuningInstanceAsyncMultiCrit"))
}

#' @export
assert_tuning_instance_async = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceAsyncSingleCrit",
    "TuningInstanceAsyncMultiCrit"))
}

#' @export
assert_tuning_instance_batch = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceBatchSingleCrit",
    "TuningInstanceBatchMultiCrit"))
}

