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
#' `$update_start()` is called in each tuning iteration before the evaluation.
#'
#' `$update_end()` is called in each tuning iteration after the evaluation.
#'
#' `$state` (`list`):
#'   Arbitrary state of the Terminator. Gets updated with each call of `update_start()` and
#'   `update_end()`.
#'
#' `$terminated` (`logical(1)`):
#'   Is the termination criterion met?
#'   Updated by each call of `update_start()`/`update_end()`.
#'
#' `$message` a meaningful message (as string) describing the state of the Terminator.
#'
#' @name TerminatorBase
#' @aliases Terminator
#' @keywords internal
#' @family Terminator
NULL

#' @export
TerminatorBase = R6Class("TerminatorBase",
  public = list(

    id = NULL,
    terminated = NULL,
    settings = NULL,
    state = NULL,

    initialize = function(id, settings) {
      self$id = assert_string(id)
      self$settings = assert_list(settings)
    },

    update_start = function(fitness_function) {
      stop("$update_start() not implemented for TerminatorBase.")
    },

    update_end = function(fitness_function) {
      stop("$update_end() not implemented for TerminatorBase.")
    }
  ),

  active = list(
    message = function() {
      stop("$message not implemented for TerminatorBase.")
    }
  )
)
