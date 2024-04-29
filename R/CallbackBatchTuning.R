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
    #'   Stage called after design is created.
    #'   Called in `ObjectiveTuning$eval_many()`.
    on_eval_after_design = NULL,

    #' @field on_eval_after_benchmark (`function()`)\cr
    #'   Stage called after hyperparameter configurations are evaluated.
    #'   Called in `ObjectiveTuning$eval_many()`.
    on_eval_after_benchmark = NULL,

    #' @field on_eval_before_archive (`function()`)\cr
    #'   Stage called before performance values are written to the archive.
    #'   Called in `ObjectiveTuning$eval_many()`.
    on_eval_before_archive = NULL
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
#'      - on_result
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
#'   Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param man (`character(1)`)\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'   The referenced help package can be opened via method `$help()`.
#' @param on_optimization_begin (`function()`)\cr
#'   Stage called at the beginning of the optimization.
#'   Called in `Optimizer$optimize()`.
#' @param on_optimizer_before_eval (`function()`)\cr
#'   Stage called after the optimizer proposes points.
#'   Called in `OptimInstance$eval_batch()`.
#' @param on_eval_after_design (`function()`)\cr
#'   Stage called after the design is created.
#'   Called in `ObjectiveTuning$eval_many()`.
#'   The context available is [ContextBatchTuning].
#' @param on_eval_after_benchmark (`function()`)\cr
#'   Stage called after hyperparameter configurations are evaluated.
#'   Called in `ObjectiveTuning$eval_many()`.
#'   The context available is [ContextBatchTuning].
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveTuning$eval_many()`.
#'   The context available is [ContextBatchTuning].
#' @param on_optimizer_after_eval (`function()`)\cr
#'   Stage called after points are evaluated.
#'   Called in `OptimInstance$eval_batch()`.
#' @param on_result (`function()`)\cr
#'   Stage called after the result is written.
#'   Called in `OptimInstance$assign_result()`.
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
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
    on_result,
    on_optimization_end),
    c(
      "on_optimization_begin",
      "on_optimizer_before_eval",
      "on_eval_after_design",
      "on_eval_after_benchmark",
      "on_eval_before_archive",
      "on_optimizer_after_eval",
      "on_result",
      "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackBatchTuning$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
