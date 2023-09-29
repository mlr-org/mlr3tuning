#' @title Create Rush Tuning Callback
#'
#' @description
#' Specialized [bbotk::CallbackOptimization] for tuning with rush.
#' Callbacks allow to customize the behavior of processes in mlr3tuning.
#' The [callback_rush_tuning()] function creates a [CallbackRushTuning].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on tuning callbacks see [callback_tuning()].
#'
#' @examples
#' # write archive to disk
#' callback_tuning("mlr3tuning.backup",
#'   on_optimization_end = function(callback, context) {
#'     saveRDS(context$instance$archive, "archive.rds")
#'   }
#' )
CallbackRushTuning = R6Class("CallbackRushTuning",
  inherit = bbotk::CallbackOptimization,
  public = list(

    #' @field on_eval_after_xs (`function()`)\cr
    #'   Stage called after xs is passed.
    #'   Called in `ObjectiveTuning$eval()`.
    on_eval_after_xs = NULL,

    #' @field on_eval_after_resample (`function()`)\cr
    #'   Stage called after hyperparameter configurations are evaluated.
    #'   Called in `ObjectiveTuning$eval()`.
    on_eval_after_resample = NULL,

    #' @field on_eval_before_archive (`function()`)\cr
    #'   Stage called before performance values are written to the archive.
    #'   Called in `ObjectiveTuning$eval()`.
    on_eval_before_archive = NULL
  )
)

#' @title Create Rush Tuning Callback
#'
#' @description
#' Function to create a [CallbackRushTuning].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Tuning callbacks can be called from different stages of tuning process.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Tuning
#'      - on_optimization_begin
#'     Start Tuner Batch
#'          - on_optimizer_before_eval
#'         Start Evaluation
#'              - on_eval_after_xs
#'              - on_eval_after_resample
#'              - on_eval_before_archive
#'         End Evaluation
#'          - on_optimizer_after_eval
#'     End Tuner Batch
#'      - on_result
#'      - on_optimization_end
#' End Tuning
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A tuning callback works with [bbotk::ContextOptimization] and [ContextEval].
#'
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#'
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Avoid writing large data the state.
#' This can slow down the tuning process when the evaluation of configurations is parallelized.
#'
#' Tuning callbacks access two different contexts depending on the stage.
#' The stages `on_eval_after_xs`, `on_eval_after_resample`, `on_eval_before_archive` access [ContextEval].
#' This context can be used to customize the evaluation of a batch of hyperparameter configurations.
#' Changes to the state of callback are lost after the evaluation of a batch and changes to the tuning instance or the tuner are not possible.
#' Persistent data should be written to the archive via `$aggregated_performance` (see [ContextEval]).
#' The other stages access [ContextOptimization].
#' This context can be used to modify the tuning instance, archive, tuner and final result.
#' There are two different contexts because the evaluation can be parallelized i.e. multiple instances of [ContextEval] exists on different workers at the same time.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param man (`character(1)`)\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'   The referenced help package can be opened via method `$help()`.
#' @param on_optimization_begin (`function()`)\cr
#'   Stage called at the beginning of the optimization.
#'   Called in `Optimizer$optimize()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_optimizer_before_eval (`function()`)\cr
#'   Stage called after the optimizer proposes points.
#'   Called in `OptimInstance$eval_batch()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_eval_after_xs (`function()`)\cr
#'   Stage called after xs is passed.
#'   Called in `ObjectiveTuning$eval()`.
#'   The context available is [ContextEval].
#' @param on_eval_after_resample (`function()`)\cr
#'   Stage called after a hyperparameter configuration is evaluated.
#'   Called in `ObjectiveTuning$eval()`.
#'   The context available is [ContextEval].
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveTuning$eval()`.
#'   The context available is [ContextEval].
#' @param on_optimizer_after_eval (`function()`)\cr
#'   Stage called after points are evaluated.
#'   Called in `OptimInstance$eval_batch()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_result (`function()`)\cr
#'   Stage called after result are written.
#'   Called in `OptimInstance$assign_result()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
#'   The context available is [bbotk::ContextOptimization].
#'
#' @export
#' @inherit CallbackRushTuning examples
callback_rush_tuning = function(id, label = NA_character_, man = NA_character_, on_optimization_begin = NULL, on_optimizer_before_eval = NULL, on_eval_after_xs = NULL, on_eval_after_resample = NULL, on_eval_before_archive = NULL, on_optimizer_after_eval = NULL, on_result = NULL,  on_optimization_end = NULL) {
  stages = discard(set_names(list(on_optimization_begin, on_optimizer_before_eval, on_eval_after_xs, on_eval_after_resample, on_eval_before_archive, on_optimizer_after_eval, on_result, on_optimization_end), c("on_optimization_begin", "on_optimizer_before_eval", "on_eval_after_xs", "on_eval_after_resample", "on_eval_before_archive", "on_optimizer_after_eval", "on_result",  "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackRushTuning$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
