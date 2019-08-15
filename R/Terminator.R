#' @title Abstract Terminator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Terminator` class that implements the main functionality each terminator must have.
#' A terminator is an object that determines when to stop the tuning.
#'
#' Termination of tuning works as follows:
#' * Evaluations in a tuner a performed in batches of size `batch_size`.
#' * After a batch-eval the Tuner is checked, if it is positive, we stop.
#' * The tuning algorithm itself might decide not to produce any more points or even a smaller batch in its last evaluation.
#'
#' Therefore the following note seems in order: While it is definitely possible to execute a fine-grained control for termination,
#' and for many tuners we can specify exactly when to stop, it might happen that a few too many or even a few too few evaluations are
#' performed, especially if your batchsize is large. So better check the size of the returned archive.
#'
#' @section Construction:
#' ```
#' t = Terminator$new(settings)
#' ```
#' * `settings` :: named `list()`\cr
#'   Arbitrary settings required by the child class.
#'
#' @section Fields:
#' * `settings` :: named `list()`\cr
#'
#' @section Methods:
#' * `is_terminated(pe)`\cr
#'   [PerfEval] -> `logical(1)`\cr
#'   Is `TRUE` iff the termination criterion is positive.
#'   Must be implemented in a subclass.
#'
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    settings = NULL,

    initialize = function(settings) {
      self$settings = assert_list(settings, names = "unique")
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* settings:", as_short_string(self$settings)))
    },

    is_terminated = function(pe) invisible(self) # overwrite in subclasses
  )
)
