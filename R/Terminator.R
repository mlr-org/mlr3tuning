#' @title Abstract Terminator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Terminator` class that implements the main functionality each terminator must have.
#' A terminator is an object that determines when to stop the tuning.
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
#'   Settings passed during construction.
#' * `terminated` :: `logical(1)`\cr
#'   Is `TRUE` if the termination criterion if the child class is met.
#' * `remaining` :: `character(1)`\cr
#'   String describing the remaining budget.
#'   Used for printing and logging.
#'
#' @section Methods:
#' * `eval_before(pe)`\cr
#'   [PerfEval] -> `self`\cr
#'   Is called in each tuning iteration before the evaluation.
#'   Can/Should be implemented in a subclass.
#' * `eval_after(pe)`\cr
#'   [PerfEval] -> `self`\cr
#'   Is called in each tuning iteration after the evaluation.
#'   Can/Should be implemented in a subclass.
#'
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    terminated = FALSE,
    settings = NULL,

    initialize = function(settings) {
      self$settings = assert_list(settings, names = "unique")
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* Terminated:", self$terminated))
      catf(str_indent("* settings:", as_short_string(self$settings)))
    },

    eval_before = function(pe) invisible(self), # overwrite these 2 in subclasses
    eval_after = function(pe) invisible(self)
  ),

  active = list(
    remaining = function() stop("abstract")
  )
)
