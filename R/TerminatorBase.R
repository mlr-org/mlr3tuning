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
#' * `settings` (`list`):
#'   The settings for this Terminator.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorBase].
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
#' @name TerminatorBase
#' @keywords internal
#' @family Terminator
NULL

#' @export
TerminatorBase = R6Class("TerminatorBase",
  public = list(

    id = NULL,
    settings = NULL,
    state = NULL,
    terminated = NULL,
  
    initialize = function(id, settings) {
      self$id = assert_string(id)
      self$settings = assert_list(settings)
    },

    update = function() {
      stop("update() not implemented for TerminatorBase.")
    },
    
  ),
  active = list(
    message = function() {
      stop("message not implemented for TerminatorBase.")
    }),
  private = list()
)