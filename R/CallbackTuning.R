#' @title Create Tuning Callback
#'
#' @description
#' Specialized [mlr3misc::Callback] for tuning.
#' Callbacks allow to customize the behavior of processes in mlr3tuning.
#' The [callback_tuning()] function creates a [CallbackTuning].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' @examples
#' # write archive to disk
#' callback_tuning("mlr3tuning.backup",
#'   on_optimization_end = function(callback, context) {
#'     saveRDS(context$instance$archive, "archive.rds")
#'   }
#' )
CallbackTuning = R6Class("CallbackTuning",
  inherit = bbotk::CallbackOptimization,
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

#' @title Create Tuning Callback
#'
#' @description
#' Function to create a [CallbackTuning].
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
#'   The functions must have two arguments named `callback` and `context`.
#' @param on_optimizer_before_eval (`function()`)\cr
#'   Stage called after the optimizer proposes points.
#'   Called in `OptimInstance$eval_batch()`.
#'   The functions must have two arguments named `callback` and `context`.
#' @param on_eval_after_design (`function()`)\cr
#'   Stage called after design is created.
#'   Called in `ObjectiveTuning$eval_many()`.
#' @param on_eval_after_benchmark (`function()`)\cr
#'   Stage called after hyperparameter configurations are evaluated.
#'   Called in `ObjectiveTuning$eval_many()`.
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveTuning$eval_many()`.
#' @param on_optimizer_after_eval (`function()`)\cr
#'   Stage called after points are evaluated.
#'   Called in `OptimInstance$eval_batch()`.
#'   The functions must have two arguments named `callback` and `context`.
#' @param on_result (`function()`)\cr
#'   Stage called after result are written.
#'   Called in `OptimInstance$assign_result()`.
#'   The functions must have two arguments named `callback` and `context`.
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
#'   The functions must have two arguments named `callback` and `context`.
#' @param fields (list of `any`)\cr
#'   List of additional fields.
#'
#' @export
#' @inherit CallbackTuning examples
callback_tuning = function(id, label = NA_character_, man = NA_character_, on_optimization_begin = NULL, on_optimizer_before_eval = NULL, on_eval_after_design = NULL, on_eval_after_benchmark = NULL, on_eval_before_archive = NULL, on_optimizer_after_eval = NULL, on_result = NULL,  on_optimization_end = NULL, fields = list()) {
  stages = discard(set_names(list(on_optimization_begin, on_optimizer_before_eval, on_eval_after_design, on_eval_after_benchmark, on_eval_before_archive, on_optimizer_after_eval, on_result, on_optimization_end), c("on_optimization_begin", "on_optimizer_before_eval", "on_eval_after_design", "on_eval_after_benchmark", "on_eval_before_archive", "on_optimizer_after_eval", "on_result",  "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackTuning$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
