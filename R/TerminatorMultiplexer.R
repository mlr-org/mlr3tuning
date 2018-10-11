#' @title TerminatorMultiplexer
#'
#' @description
#' TerminatorMultiplexer takes multiple Terminators and will lead to termination as soon as one Terminator signals a termination.
#' In the future more advanced termination rules can be implemented using this Multiplexer.
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
#' * `settings` (`list`):
#'   The settings for this Terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorMultiplexer].
#' 
#' `$update()` is called in each tuning iteration before the evaluation.
#' 
#' `$state` (`list`):
#'   Custom state of the Terminator.
#'   Individual for each subclass.
#'   Gets updated with each call of `update()`.
#' 
#' `$terminated` (`logical(1)`):
#'   Updated by each call of `update()`.
#'   Is the termination criterion met?
#' 
#' `$message` a meaningfull chararacter string describing the state of the Terminator.
#'   
#' @name TerminatorMultiplexer
#' @keywords internal
#' @family Terminator
NULL

#' @export
TerminatorMultiplexer = R6Class("TerminatorMultiplexer",
  inherit = TerminatorBase,
  terminators = NULL,

  public = list(
    initialize = function(id, terminators) {
      self$terminators = assert_list(terminators, types = "TerminatorBase")
      supper$initialize(id = id, settings = list())
    },

    update_start = function(fitness_function) {
      if (is.null(self$state)) {
        self$terminated = FALSE
        self$state = list(terminated.inds = NULL)
      }
      for (i in seq_along(terminators)) {
        terminator = self$terminators[[i]]
        terminator$update_start(fitness_function)
        self$state$terminated.inds = c(self$state$terminated.inds, i)
        self$terminated = self$terminated || terminator$terminated
        invisible(self$terminated)
      }
    },

    update_end = function(fitness_function) {
      for (i in seq_along(terminators)) {
        terminator = self$terminators[[i]]
        terminator$update_end(fitness_function)
        self$state$terminated.inds = c(self$state$terminated.inds, i)
        self$terminated = self$terminated || terminator$terminated
        invisible(self$terminated)
      }
    }
    
  ),
  active = list(
    message = function() {
      if (self$terminated) {
        msgs = vapply(self$state$terminated.inds, function(i) {
          paste0(self$terminators[[i]]$id, ": ", self$terminators[[i]]$terminated, character(1L))
        })
        paste0("Terminated: ", msgs, collapse = ", ")
      } else {
        "No Terminator terminated."
      }
    }
  ),
  private = list()
)