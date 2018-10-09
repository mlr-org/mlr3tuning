#' @title TerminatorBase
#'
#' @description
#' TerminatorBase.
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
#' `$new()` creates a new object of class [TerminatorBase].
#'
#' @name TerminatorBase
#' @keywords internal
#' @family TerminatorBase
NULL

#' @export
TerminatorBase = R6Class("TerminatorBase",
  public = list(
  
    initialize = function(id) {
    
    }

    start = function() {
      stop("start() not implemented for TerminatorBase.")
    }

    is_terminated(tuner_base) {
      stop("is_terminated() not implemented fot TerminatorBase.")
    }
    
  ),
  active = list(),
  private = list()
)