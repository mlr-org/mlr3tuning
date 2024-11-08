#' @title Create Batch Tuning Callback
#'
#' @description
#' Specialized [bbotk::CallbackBatch] for batch tuning.
#' Callbacks allow to customize the behavior of processes in mlr3tuning.
#' The [callback_batch_tuning()] function creates a [CallbackBatchTuning].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on tuning callbacks see [callback_batch_tuning()].
#'
#' @export
#' @examples
#' # write archive to disk
#' callback_batch_tuning("mlr3tuning.backup",
#'   on_optimization_end = function(callback, context) {
#'     saveRDS(context$instance$archive, "archive.rds")
#'   }
#' )
CallbackBatchTuning= R6Class("CallbackBatchTuning",
  inherit = CallbackBatch,
  public = list(

    #' @field on_eval_after_design (`function()`)\cr
    #' Stage called after design is created.
    #' Called in `ObjectiveTuningBatch$eval_many()`.
    on_eval_after_design = NULL,

    #' @field on_eval_after_benchmark (`function()`)\cr
    #' Stage called after hyperparameter configurations are evaluated.
    #' Called in `ObjectiveTuningBatch$eval_many()`.
    on_eval_after_benchmark = NULL,

    #' @field on_eval_before_archive (`function()`)\cr
    #' Stage called before performance values are written to the archive.
    #' Called in `ObjectiveTuningBatch$eval_many()`.
    on_eval_before_archive = NULL,

    #' @field on_tuning_result_begin (`function()`)\cr
    #' Stage called before the results are written.
    #' Called in `TuningInstance*$assign_result()`.
    on_tuning_result_begin = NULL
  )
)

#' @title Create Batch Tuning Callback
#'
#' @description
#' Function to create a [CallbackBatchTuning].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Tuning callbacks can be called from different stages of the tuning process.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Tuning
#'      - on_optimization_begin
#'     Start Tuner Batch
#'          - on_optimizer_before_eval
#'         Start Evaluation
#'              - on_eval_after_design
#'              - on_eval_after_benchmark
#'              - on_eval_before_archive
#'         End Evaluation
#'          - on_optimizer_after_eval
#'     End Tuner Batch
#'      - on_tuning_result_begin
#'      - on_result_begin
#'      - on_result_end
#'      - on_optimization_end
#' End Tuning
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A tuning callback works with [ContextBatchTuning].
#'
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Tuning callbacks access [ContextBatchTuning].
#'
#' @param id (`character(1)`)\cr
#'  Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'  Label for the new instance.
#' @param man (`character(1)`)\cr
#'  String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'  The referenced help package can be opened via method `$help()`.
#'
#' @param on_optimization_begin (`function()`)\cr
#'  Stage called at the beginning of the optimization.
#'  Called in `Optimizer$optimize()`.
#'  The functions must have two arguments named `callback` and `context`.
#' @param on_optimizer_before_eval (`function()`)\cr
#'  Stage called after the optimizer proposes points.
#'  Called in `OptimInstance$eval_batch()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The argument of `$eval_batch(xdt)` is available in `context`.
#' @param on_eval_after_design (`function()`)\cr
#'  Stage called after the design is created.
#'  Called in `ObjectiveTuningBatch$eval_many()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The arguments of `$eval_many(xss, resampling)` are available in `context`.
#'  Additionally, the `design` is available in `context`.
#' @param on_eval_after_benchmark (`function()`)\cr
#'  Stage called after hyperparameter configurations are evaluated.
#'  Called in `ObjectiveTuningBatch$eval_many()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The `benchmark_result` is available in `context`.
#' @param on_eval_before_archive (`function()`)\cr
#'  Stage called before performance values are written to the archive.
#'  Called in `ObjectiveTuningBatch$eval_many()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The `aggregated_performance` is available in `context`.
#' @param on_optimizer_after_eval (`function()`)\cr
#'  Stage called after points are evaluated.
#'  Called in `OptimInstance$eval_batch()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The new configurations and performances in `instance$archive` are available in `context`.
#' @param on_tuning_result_begin (`function()`)\cr
#'  Stage called at the beginning of the result writing.
#'  Called in `TuningInstanceBatch$assign_result()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The arguments of `$assign_result(xdt, y, learner_param_vals, extra)` are available in `context`.
#' @param on_result_begin (`function()`)\cr
#'  Stage called at the beginning of the result writing.
#'  Called in `OptimInstance$assign_result()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The arguments of `$assign_result(xdt, y, extra)` are available in `context`.
#' @param on_result_end (`function()`)\cr
#'  Stage called after the result is written.
#'  Called in `OptimInstance$assign_result()`.
#'  The functions must have two arguments named `callback` and `context`.
#'  The final result `instance$result` is available in `context`.
#' @param on_result (`function()`)\cr
#'  Deprecated.
#'  Use `on_result_end` instead.
#'  Stage called after the result is written.
#'  Called in `OptimInstance$assign_result()`.
#'  The functions must have two arguments named `callback` and `context`.
#' @param on_optimization_end (`function()`)\cr
#'  Stage called at the end of the optimization.
#'  Called in `Optimizer$optimize()`.
#'  The functions must have two arguments named `callback` and `context`.
#'
#' @export
#' @inherit CallbackBatchTuning examples
callback_batch_tuning = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimization_begin = NULL,
  on_optimizer_before_eval = NULL,
  on_eval_after_design = NULL,
  on_eval_after_benchmark = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
  on_tuning_result_begin = NULL,
  on_result_begin = NULL,
  on_result_end = NULL,
  on_result = NULL,
  on_optimization_end = NULL
  ) {
  stages = discard(set_names(list(
    on_optimization_begin,
    on_optimizer_before_eval,
    on_eval_after_design,
    on_eval_after_benchmark,
    on_eval_before_archive,
    on_optimizer_after_eval,
    on_tuning_result_begin,
    on_result_begin,
    on_result_end,
    on_result,
    on_optimization_end),
    c(
      "on_optimization_begin",
      "on_optimizer_before_eval",
      "on_eval_after_design",
      "on_eval_after_benchmark",
      "on_eval_before_archive",
      "on_optimizer_after_eval",
      "on_tuning_result_begin",
      "on_result_begin",
      "on_result_end",
      "on_result",
      "on_optimization_end")), is.null)

  if ("on_result" %in% names(stages)) {
    .Deprecated(old = "on_result", new = "on_result_end")
    stages$on_result_end = stages$on_result
    stages$on_result = NULL
  }

  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackBatchTuning$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}

#' @title Assertions for Callbacks
#'
#' @description
#' Assertions for [CallbackBatchTuning] class.
#'
#' @param callback ([CallbackBatchTuning]).
#' @param null_ok (`logical(1)`)\cr
#'   If `TRUE`, `NULL` is allowed.
#'
#' @return [CallbackBatchTuning | List of [CallbackBatchTuning]s.
#' @export
assert_batch_tuning_callback = function(callback, null_ok = FALSE) {
  if (null_ok && is.null(callback)) return(invisible(NULL))
  assert_class(callback, "CallbackBatchTuning")
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackBatchTuning]).
#' @rdname assert_batch_tuning_callback
assert_batch_tuning_callbacks = function(callbacks) {
  invisible(lapply(callbacks, assert_callback))
}
