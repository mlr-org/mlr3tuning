#' @title Create Async Tuning Callback
#'
#' @description
#' Specialized [bbotk::CallbackAsync] for tuning.
#' Callbacks allow to customize the behavior of processes in mlr3tuning.
#' The [callback_async_tuning()] function creates a [CallbackTuningAsync].
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
CallbackTuningAsync = R6Class("CallbackTuningAsync",
  inherit = bbotk::CallbackAsync,
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

#' @title Create Async Tuning Callback
#'
#' @description
#' Function to create a [CallbackTuningAsync].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Tuning callbacks can be called from different stages of tuning process.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Tuning
#'      - on_optimization_begin
#'     Start Worker
#'          - on_optimizer_begin
#'         Start Evaluation
#'              - on_eval_after_xs
#'              - on_eval_after_resample
#'              - on_eval_before_archive
#'         End Evaluation
#'          - on_optimizer_end
#'     End Worker
#'      - on_result
#'      - on_optimization_end
#' End Tuning
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A tuning callback works with [bbotk::ContextOptimization] and [ContextAsyncTuning].
#'
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
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
#' @param on_optimizer_begin (`function()`)\cr
#'   Stage called at the beginning of the optimization on the worker.
#'   Called in the worker loop.
#' @param on_eval_after_xs (`function()`)\cr
#'   Stage called after xs is passed.
#'   Called in `ObjectiveTuning$eval()`.
#' @param on_eval_after_resample (`function()`)\cr
#'   Stage called after a hyperparameter configuration is evaluated.
#'   Called in `ObjectiveTuning$eval()`.
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveTuning$eval()`.
#' @param on_optimizer_end (`function()`)\cr
#'   Stage called at the end of the optimization on the worker.
#'   Called in the worker loop.
#' @param on_result (`function()`)\cr
#'   Stage called after result are written.
#'   Called in `OptimInstance$assign_result()`.
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
#'
#' @export
#' @inherit CallbackTuningAsync examples
callback_async_tuning = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimization_begin = NULL,
  on_optimizer_begin = NULL,
  on_eval_after_xs = NULL,
  on_eval_after_resample = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
  on_result = NULL,
  on_optimization_end = NULL
  ) {
  stages = discard(set_names(list(
    on_optimization_begin,
    on_optimizer_begin,
    on_eval_after_xs,
    on_eval_after_resample,
    on_eval_before_archive,
    on_optimizer_end,
    on_result,
    on_optimization_end),
    c("on_optimization_begin",
    "on_optimizer_before_eval",
    "on_eval_after_xs",
    "on_eval_after_resample",
    "on_eval_before_archive",
    "on_optimizer_end",
    "on_result",
    "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackTuningAsync$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
