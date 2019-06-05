#' @title TerminatorPerformance Class
#'
#' @description
#' Class to terminate the tuning after some time. Note that the runtime is checked after each step
#' and therefore it could happen that the final runtime is longer than the specified one. Time is measured
#' for everything that happens between update_start and update_end.
#'
#' @section Usage:
#' ```
#' # Constructor
#' t = TerminatorPerformance$new(thresh, pe, all_reached = FALSE)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `thresh` (named list):\cr
#'   Thresholds that needs to be reached.
#' * `pe` ([PerformanceEvaluator]):\cr
#'   Performance evaluator used for the tuning. This is required to check whether the defined thresholds makes sense or not.
#' * `all_reached` (logical(1)):\cr
#'   Stop whether all thresholds are reached (default = FALSE).
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorRuntime].
#'
#' The interface is described in [Terminator].
#'
#' @name TerminatorPerformance
#' @family Terminator
#' @examples
#' \donttest{
#' t = TerminatorPerformance$new(0.5, pe)
#' }
NULL

#' @export
#' @include Terminator.R
TerminatorPerformance = R6Class("TerminatorPerformance",
  inherit = Terminator,
  public = list(

    initialize = function(thresh, pe, all_reached = FALSE) {
      checkmate::assert_r6(pe, "PerformanceEvaluator")
      checkmate::assert_names(names(thresh), subset.of = names(map(pe$task$measures, "id")))
      checkmate::assert_logical(all_reached, len = 1)
      imap(thresh, function(th, i) {
        checkmate::assert_double(th, len = 1, lower = pe$task$measures[[i]]$range[1], upper = pe$task$measures[[i]]$range[2])
      })
      super$initialize(settings = list(thresh = thresh, all_reached = all_reached))

      self$terminated = FALSE
      self$state = list(msrs_best = list())
    },

    update_start = function(pe) {
      invisible(self)
    },

    update_end = function(pe) {
      aggr = pe$bmr$aggregated()
      thresh_reached = imap(self$settings$thresh, function(th, i) {
        perfs = aggr[[i]]
        if (pe$task$measures[[i]]$minimize) {
          self$state$msrs_best[i] = min(perfs)
          return(min(perfs) <= th)
        } else {
          self$state$msrs_best[i] = max(perfs)
          return(max(perfs) >= th)
        }
      })
      if (self$settings$all_reached) {
        if (all(unlist(thresh_reached))) {
          self$terminated = TRUE
        }
      } else {
        if (any(unlist(thresh_reached))) {
          self$terminated = TRUE
        }
      }
      invisible(self)
    },

    format = function() {
      state = paste(paste0(names(self$settings$thresh), " = ", round(unlist(self$state$msrs_best), 4)), collapse = ", ")
      sprintf("TerminatorPerformance with current values %s.", state)
    }
  )
)
