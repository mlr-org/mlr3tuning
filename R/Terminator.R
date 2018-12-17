#' @title Terminator Base Class
#'
#' @description
#' Base class for termination criteria.
#'
#' @section Usage:
#' ```
#' # Construction
#' t = Terminator$new(settings)
#' 
#' # public members
#' t$terminated
#' t$settings
#' t$state
#' 
#' # public methods
#' t$update_start(ff)
#' t$update_end(ff)
#' 
#' # active bindings
#' t$remaining
#' ```
#'
#' @section Arguments:
#' * `settings` (`list`):
#'   The settings for Terminator as named list.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Terminator].
#' * `$update_start()` is called in each tuning iteration before the evaluation.
#' * `$update_end()` is called in each tuning iteration after the evaluation.
#' * `$state` (`list`):
#'    Arbitrary state of the Terminator. Gets updated with each call of `update_start()` and `update_end()`.
#' * `$terminated` (`logical(1)`):
#'    Is the termination criterion met?
#'    Updated by each call of `update_start()`/`update_end()`.
#'
#' @name Terminator
#' @family Terminator
NULL

#' @export
Terminator = R6Class("Terminator",
  public = list(
    terminated = NULL,
    settings = NULL,
    state = NULL,

    initialize = function(settings) { self$settings = assert_list(settings, names = "unique") },
    update_start = function(ff) { stop("$update_start() not implemented for Terminator") },
    update_end = function(ff) { stop("$update_end() not implemented for Terminator") }
  ),

  active = list(
    remaining = function() { NA_integer_ }
  )
)
