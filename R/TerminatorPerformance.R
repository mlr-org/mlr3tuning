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
#' t = TerminatorPerformance$new(thresh, pe)
#' ```
#' * `thresh` :: named `numeric()`\cr
#'   Thresholds that need to be reached, named with measure ids.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or falls below (respective measure has to be minimized) all provided threshold simultaneously.
#' * `pe` :: ([PerformanceEvaluator])\cr
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
TerminatorPerformance = R6Class("TerminatorPerformance",
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
      self$terminated = FALSE
    },

    update_start = function(pe) {
      invisible(self)
    },

    update_end = function(pe) {
      thresh = self$settings$thresh
      measures = pe$measures[match(map_chr(pe$measures, "id"), names(thresh))]
      minimize = ifelse(map_lgl(measures, "minimize"), 1, -1)

      aggr = pe$bmr$aggregate(pe$measures)[, names(measures), with = FALSE]
      reached = pmap_lgl(list(aggr = aggr, thresh = thresh, measure = measures),
        function(aggr, thresh, measure) {
          all(if (measure$minimize) aggr <= thresh else aggr >= thresh)
        }
      )

      if (any(reached)) {
        self$terminated = TRUE
      }

      invisible(self)
    },

    print = function() {
      catf("%s with goal: %s", format(self), as_short_string(as.list(self$settings$thresh)))
    }

  )
)
