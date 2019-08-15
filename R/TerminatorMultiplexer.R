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
#'   TerminatorEvals$new(10)
#' ))
#' print(t)
TerminatorMultiplexer = R6Class("TerminatorMultiplexer",
  inherit = Terminator,

  public = list(
    terminators = NULL,
    initialize = function(terminators) {
      self$terminators = assert_list(terminators, types = "Terminator")
      super$initialize(list())
    },

    is_terminated = function(pe) {
      any(map_lgl(self$terminators, function(t) t$is_terminated(pe)))
    }
  )
)
