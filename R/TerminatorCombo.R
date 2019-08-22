#' @title Combine Terminators
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' This class takes multiple [Terminator]s and terminates as soon as one or all of the included terminators are positive.
#'
#' @section Construction:
#' ```
#' t = TerminatorCombo$new(terminators, any = TRUE)
#' ```
#' * `terminators` :: `list()`\cr
#'   List of objects of class [Terminator].
#' * `any` :: `logical(1)`\cr
#'   Terminate iff any included terminator is positive? (not all).
#'   Stored in `settings`.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorCombo$new(list(
#'   TerminatorModelTime$new(60),
#'   TerminatorEvals$new(10)
#' ))
TerminatorCombo = R6Class("TerminatorSet",
  inherit = Terminator,

  public = list(
    terminators = NULL,

    initialize = function(terminators, any = TRUE) {
      self$terminators = assert_list(terminators, types = "Terminator")
      super$initialize(settings = list(any = assert_flag(any)))
    },

    is_terminated = function(pe) {
      g = if (self$settings$any) any else all
      g(map_lgl(self$terminators, function(t) t$is_terminated(pe)))
    }
  )
)
