#' @title Terminator that never stops.
#'
#' @aliases mlr_terminators_none
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Mainly useful for grid search, or maybe other tuners, where the stopping is inherently controlled by the tuner itself.
#'
#' @section Construction:
#' ```
#' t = TerminatorNone$new()
#' term("none")
#' ```
#'
#' @family Terminator
#' @export
TerminatorNone = R6Class("TerminatorNone",
  inherit = Terminator,
  public = list(

    initialize = function() {
      super$initialize()
    },

    is_terminated = function(instance) return(FALSE)
  )
)

mlr_terminators$add("none", TerminatorNone)
