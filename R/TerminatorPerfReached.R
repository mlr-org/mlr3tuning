#' @title Terminator with Performance Criterion
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a performance threshold has been hit.
#'
#' @section Construction:
#' ```
#' t = TerminatorPerfReached$new(thresh, pe)
#' ```
#' * `thresh` :: named `numeric()`\cr
#'   Thresholds that need to be reached, named with measure ids.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or falls below (respective measure has to be minimized) all provided threshold simultaneously.
#' * `pe` :: [PerformanceEvaluator]\cr
#'   Performance evaluator used for the tuning.
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function(thresh, pe, all_reached = FALSE) {
      assert_r6(pe, "PerformanceEvaluator")
      measures = set_names(pe$measures, map_chr(pe$measures, "id"))
      assert_numeric(thresh, names = "unique", any.missing = FALSE)
      assert_names(names(thresh), subset.of = names(measures))
      imap(thresh, function(th, id) {
        assert_number(th, lower = measures[[id]]$range[1L], upper = measures[[id]]$range[2L])
      })
      assert_flag(all_reached)

      super$initialize(settings = list(thresh = thresh, all_reached = all_reached))
      self$state$delta = set_names(rep(Inf, length(thresh)), names(thresh))
      self$terminated = FALSE
    },

    update_start = function(pe) {
      invisible(self)
    },

    update_end = function(pe) {
      thresh = self$settings$thresh
      measures = pe$measures[match(map_chr(pe$measures, "id"), names(thresh))]

      aggr = pe$bmr$aggregate(pe$measures)[, names(measures), with = FALSE]
      delta = pmap_dbl(list(aggr = aggr, thresh = thresh, measure = measures),
        function(aggr, thresh, measure) {
          if (measure$minimize) min(aggr - thresh) else max(thresh - aggr)
        }
      )
      names(delta) = names(thresh)
      self$state$delta = delta


      if (any(delta < 0)) {
        self$terminated = TRUE
      }

      invisible(self)
    },

    print = function() {
      catf("%s (remaining: %s)", format(self), self$remaining)
    }
  ),

  active = list(
    remaining = function() {
      delta = self$state$delta
      paste0(sprintf("%s: %.3f", names(delta), delta), collapse = ", ")
    }
  )
)
