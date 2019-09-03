#' @title Abstract Terminator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_terminators.R
#'
#' @description
#' Abstract `Terminator` class that implements the main functionality each terminator must have.
#' A terminator is an object that determines when to stop the tuning.
#'
#' Termination of tuning works as follows:
#' * Evaluations in a tuner are performed in batches.
#' * Before and after a batch evaluation the [Terminator] is checked, if it is positive, we stop.
#' * The tuning algorithm itself might decide not to produce any more points, or even might decide to do a smaller batch in its last evaluation.
#'
#' Therefore the following note seems in order:
#' While it is definitely possible to execute a fine-grained control for termination, and for many tuners we can specify exactly when to stop,
#' it might happen that a few too many or even a few too few evaluations are performed, especially if the batch size is larger than 1.
#' So better check the size of the returned archive.
#'
#' @section Construction:
#' ```
#' t = Terminator$new(param_set = ParamSet$new(), param_vals = list())
#' ```
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of control parameters for terminator.
#'
#' * `param_vals` :: named `list()`\cr
#'   Settings of control parameters for terminator.
#'
#' @section Fields:
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'
#' @section Methods:
#' * `is_terminated(inst)`\cr
#'   [TuningInstance] -> `logical(1)`\cr
#'   Is `TRUE` iff the termination criterion is positive.
#'   Must be implemented in a subclass.
#'
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    param_set = NULL,

    initialize = function(param_set = ParamSet$new(), param_vals = list()) {
      self$param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(self$format())
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    },

    is_terminated = function(inst) TRUE # overwrite in subclasses
  )
)
