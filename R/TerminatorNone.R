#' @title Terminator that never stops.
#'
#' @name mlr_terminators_none
#' @include Terminator.R
#'
#' @description
#' Mainly useful for grid search, or maybe other tuners, where the stopping is inherently controlled by the tuner itself.
#'
#' @family Terminator
#' @export
TerminatorNone = R6Class("TerminatorNone",
  inherit = Terminator,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
    },

    #' @description
    #' Is `TRUE` iff the termination criterion is positive, and `FALSE` otherwise.
    #'
    #' @param instance ([TuningInstance]).
    #'
    #' @return `logical(1)`.
    is_terminated = function(instance) return(FALSE)
  )
)

mlr_terminators$add("none", TerminatorNone)
