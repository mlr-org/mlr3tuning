#' @title Abstract Terminator Class
#'
#' @description
#' Abstract `Terminator` class that implements the main functionality each terminator must have. A terminator is 
#' an object that says when to stop the tuning.
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
#' * `settings` (`list()`):
#'   The settings for Terminator as named list.
#' * `ff` (`FitnessFunction`):
#'   `FitnessFunction` object used to determine when to stop the tuning.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Terminator].
#' * `$terminated` (`logical(1)`) is the termination criterion met? Updated by each call of `update_start()`/`update_end()`.
#' * `$settings` (`list()`) settings that are set by the child classes to define stopping criteria.
#' * `$state` (`list()`) arbitrary state of the Terminator. Gets updated with each call of `update_start()` and `update_end()`.
#' * `$update_start()` is called in each tuning iteration before the evaluation.
#' * `$update_end()` is called in each tuning iteration after the evaluation.
#' * `$remaining` returns a list of remaining resources till the tuning is finished.
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
  )
)
