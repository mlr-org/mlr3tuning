#' @title TerminatorMultiplexer Class
#'
#' @description
#' TerminatorMultiplexer takes multiple `Terminator`s and will lead to termination as soon as one Terminator signals a termination.
#' In the future more advanced termination rules can be implemented using this Multiplexer.
#'
#' @section Usage:
#' ```
#' t = TerminatorMultiplexer$new(terminators)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `terminators` (`list`):
#'   List of objects of class [Terminator].
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorMultiplexer].
#'
#' The interface is described in [Terminator].
#'
#' @name TerminatorMultiplexer
#' @family Terminator
#' @examples
#' t = TerminatorMultiplexer$new(list(
#'   TerminatorRuntime$new(3, "mins"),
#'   TerminatorEvaluations$new(10)
#' ))
#' print(t)
NULL

#' @export
#' @include Terminator.R
TerminatorMultiplexer = R6Class("TerminatorMultiplexer",
  inherit = Terminator,

  public = list(
    terminators = NULL,
    initialize = function(terminators) {
      self$terminators = assert_list(terminators, types = "Terminator")
      self$terminated = FALSE
      super$initialize(settings = do.call("c", lapply(self$terminators, function(t) t$settings)))
    },

    update_start = function(ff) {
      lapply(self$terminators, function(t) t$update_start(ff))
      self$terminated = self$terminated || some(self$terminators, "terminated")
      invisible(self)
    },

    update_end = function(ff) {
      lapply(self$terminators, function(t) t$update_end(ff))
      self$terminated = self$terminated || some(self$terminators, "terminated")
      invisible(self)
    },

    format = function() {
      paste0(map_chr(self$terminators, format), collapse = "\n")
    }
  )
)
