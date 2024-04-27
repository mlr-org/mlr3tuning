#' @title Assertion for mlr3tuning objects
#'
#' @description
#' Most assertion functions ensure the right class attribute, and optionally additional properties.
#'
#' @name mlr3tuning_assertions
#' @keywords internal
NULL

#' @export
#' @param tuner ([Tuner]).
#' @rdname mlr3tuning_assertions
assert_tuner = function(tuner) {
  assert_r6(tuner, "Tuner")
}

#' @export
#' @param tuners (list of [Tuner]).
#' @rdname mlr3tuning_assertions
assert_tuners = function(tuners) {
  invisible(lapply(tuners, assert_tuner))
}

#' @export
#' @param tuner ([TunerAsync]).
#' @rdname mlr3tuning_assertions
assert_tuner_async = function(tuner) {
  assert_r6(tuner, "TunerAsync")
}

#' @export
#' @param tuner ([TunerBatch]).
#' @rdname mlr3tuning_assertions
assert_tuner_batch = function(tuner) {
  assert_r6(tuner, "TunerBatch")
}

#' @export
#' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit] | [TuningInstanceAsyncSingleCrit] | [TuningInstanceAsyncMultiCrit]).
#' @rdname mlr3tuning_assertions
assert_tuning_instance = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceBatchSingleCrit",
    "TuningInstanceBatchMultiCrit",
    "TuningInstanceAsyncSingleCrit",
    "TuningInstanceAsyncMultiCrit"))
}

#' @export
#' @param inst ([TuningInstanceAsyncSingleCrit] | [TuningInstanceAsyncMultiCrit]).
#' @rdname mlr3tuning_assertions
assert_tuning_instance_async = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceAsyncSingleCrit",
    "TuningInstanceAsyncMultiCrit"))
}

#' @export
#' @param inst ([TuningInstanceBatchSingleCrit] | [TuningInstanceBatchMultiCrit]).
#' @rdname mlr3tuning_assertions
assert_tuning_instance_batch = function(inst) {
  assert_multi_class(inst, c(
    "TuningInstanceBatchSingleCrit",
    "TuningInstanceBatchMultiCrit"))
}

