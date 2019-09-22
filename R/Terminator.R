#' @title Abstract Terminator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_terminators.R
#'
#' @description
#' Abstract `Terminator` class that implements the base functionality each terminator must provide.
#' A terminator is an object that determines when to stop the tuning.
#'
#' Termination of tuning works as follows:
#' * Evaluations in a tuner are performed in batches.
#' * Before and after each batch evaluation, the [Terminator] is checked, and if it is positive, we stop.
#' * The tuning algorithm itself might decide not to produce any more points, or even might decide to do a smaller batch in its last evaluation.
#'
#' Therefore the following note seems in order:
#' While it is definitely possible to execute a fine-grained control for termination, and for many tuners we can specify exactly when to stop,
#' it might happen that too few or even too many evaluations are performed, especially if multiple points are evaluated in a single batch (c.f. batch size parameter of many tuners).
#' So it is advised to check the size of the returned archive, in particular if you are benchmarking multiple tuners.
#'
#' @section Construction:
#' ```
#' t = Terminator$new(param_set = ParamSet$new())
#' ```
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of control parameters for terminator.
#'
#' @section Fields:
#'
#' * `param_set` :: [paradox::ParamSet]; from construction.
#'
#' @section Methods:
#' * `is_terminated(instance)`\cr
#'   [TuningInstance] -> `logical(1)`\cr
#'   Is `TRUE` iff the termination criterion is positive, and `FALSE` otherwise.
#'   Must be implemented in each subclass.
#'
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    param_set = NULL,

    initialize = function(param_set = ParamSet$new()) {
      self$param_set = assert_param_set(param_set)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(self$format())
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    },

    is_terminated = function(instance) TRUE # overwrite in subclasses
  )
)
