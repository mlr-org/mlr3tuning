#' @title Combine Terminators
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' This class takes multiple [Terminator]s and terminates as soon as one of the subordinate Terminators signals a termination.
#'
#' @section Construction:
#' ```
#' t = TerminatorMultiplexer$new(terminators)
#' ```
#' * `terminators` :: `list()`\cr
#'   List of objects of class [Terminator].
#'
#' @family Terminator
#' @export
#' @examples
#' t = TerminatorMultiplexer$new(list(
#'   TerminatorRuntime$new(60),
#'   TerminatorEvaluations$new(10)
#' ))
#' print(t)
TerminatorMultiplexer = R6Class("TerminatorMultiplexer",
  inherit = Terminator,

  public = list(
    terminators = NULL,
    initialize = function(terminators) {
      self$terminators = assert_list(terminators, types = "Terminator")
      self$terminated = FALSE
      super$initialize(settings = do.call("c", lapply(self$terminators, function(t) t$settings)))
    },

    update_start = function(pe) {
      lapply(self$terminators, function(t) t$update_start(pe))
      self$terminated = self$terminated || some(self$terminators, "terminated")
      invisible(self)
    },

    update_end = function(pe) {
      lapply(self$terminators, function(t) t$update_end(pe))
      self$terminated = self$terminated || some(self$terminators, "terminated")
      invisible(self)
    }
  ),

  active = list(
    remaining = function() {
      paste0(map_chr(self$terminators, "remaining"), collapse = ", ")
    }
  )
)
