#' @title TerminatorIterations
#'
#' @description
#' TerminatorIterations.
#'
#' @section Usage:
#' ```
#' l = Terminator(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The name of the Terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorIterations].
#'
#' @name TerminatorIterations
#' @keywords internal
#' @family Terminator
NULL

#' @export
TerminatorIterations = R6Class("TerminatorIterations",
  inherit = TerminatorBase,
  public = list(

    initialize = function(id, max_iterations) {
      super$initialize(id = id, settings = list(max_iterations = assert_int(max_iterations, lower = 1)))
    },

    update_start = function(fitness_function) {
      if (is.null(self$state)) {
        self$terminated = FALSE
        self$state = list(iters = 0)
      }
      invisible(self$terminated)
    },

    update_end = function(fitness_function) {
      self$state$iters = self$state$iters + 1
      self$terminated = (self$state$iters >= self$settings$max_iterations)
      invisible(self$terminated)
    }
  ),
  active = list(
    message = function() {
      if (self$terminated) {
        sprintf("Budget of %i iteratons exhausted with %i iteratons", self$settings$max_iterations, self$state$evals)
      } else {
        sprintf("Budget of %i iteratons not exhausted with %i iteratons", self$settings$max_iterations, self$state$evals)
      }
    }
  ),
  private = list()
)