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
#' * `state` :: `list()`\cr
#'   Arbitrary state, depending on the child class.
#'
#' @section Methods:
#' * `update_start(pe)`\cr
#'   [PerformanceEvaluator] -> `self`\cr
#'   Is called in each tuning iteration before the evaluation.
#' * `update_end(pe)`\cr
#'   [PerformanceEvaluator] -> `self`\cr
#'   Is called in each tuning iteration after the evaluation.
#'
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    terminated = NULL,
    settings = NULL,
    state = NULL,

    initialize = function(settings) {
      self$settings = assert_list(settings, names = "unique")
    },
    update_start = function(pe) {
      stop("$update_start() not implemented for Terminator")
    },
    update_end = function(pe) {
      stop("$update_end() not implemented for Terminator")
    },
    format = function() {
      sprintf("<%s>", class(self)[1L])
    }
  )
)
