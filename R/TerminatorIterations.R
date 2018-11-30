#' @title TerminatorIterations
#'
#' @description
#' TerminatorIterations.
#'
#' @section Usage:
#' ```
#' t = TerminatorIterations$new(max_iterations)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `max_iterations` (`integer(1)):\cr
#'   Maximum number of iterations.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorIterations].
#' The interface is described in [Terminator].
#'
#' @name TerminatorIterations
#' @family Terminator
#' @examples
#' t = TerminatorIterations$new(3)
NULL

#' @export
#' @include Terminator.R
TerminatorIterations = R6Class("TerminatorIterations",
  inherit = Terminator,
  public = list(

    initialize = function(max_iterations) {
      super$initialize(settings = list(max_iterations = assert_int(max_iterations, lower = 1L, coerce = TRUE)))
      self$terminated = FALSE
      self$state = list(iters = 0L)
    },

    update_start = function(ff) {
      invisible(self)
    },

    update_end = function(ff) {
      dob = NULL
      self$state$iters = if (nrow(ff$bmr$data) == 0L) 0L else ff$bmr$data[, max(dob)]
      self$terminated = self$state$iters >= self$settings$max_iterations
      invisible(self)
    },

    format = function() sprintf("TerminatorIterations with %i remaining iterations", self$remaining)
  ),

  active = list(
    remaining = function() {
      max(self$settings$max_iterations - self$state$iters, 0L)
    }
  )
)
