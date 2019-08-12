#' @title Terminator that never stops.
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Mainly useful for grid search, or maybe other tuners, where the stopping is inherently controlled by the tuner itself.
#'
#' @section Construction:
#' ```
#' t = TerminatorNone$new()
#' ```
#'
#' @family Terminator
#' @export
TerminatorNone = R6Class("TerminatorNone",
  inherit = Terminator,
  public = list(

    initialize = function() {
      super$initialize(settings = list())
    }
  )
)
