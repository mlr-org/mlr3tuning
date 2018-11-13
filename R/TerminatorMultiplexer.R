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

  public = list(
    terminators = NULL,
    initialize = function(id, terminators) {
      self$terminators = assert_list(terminators, types = "TerminatorBase")
      self$terminated = FALSE
      super$initialize(id = id, settings = list())
    },

    update_start = function(ff) {
      lapply(self$terminators, function(t) t$update_start(ff))
      self$terminated = self$terminated | any(vapply(self$terminators, function(t) t$terminated, NA))
      invisible(self)
    },

    update_end = function(ff) {
      lapply(self$terminators, function(t) t$update_end(ff))
      self$terminated = self$terminated | any(vapply(self$terminators, function(t) t$terminated, NA))
      invisible(self)
    }
  ),

  active = list(
    remaining = function() {
      as.integer(max(min(vapply(self$terminators, function(t) t$remaining, NA_real_)), 0))
    },
    message = function() {
      paste0(vapply(self$terminators, function(x) x$message, NA_character_), collapse = "\n")
    }
  )
)
