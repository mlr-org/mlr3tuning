#' @title Terminator that stops when a peformance threshold has been reached
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
#' t = TerminatorPerfReached$new(thresh)
#' ```
#' * `thresh` :: named `numeric()`\cr
#'   Thresholds that need to be reached, named with measure ids.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or falls below (respective measure has to be minimized) all provided threshold simultaneously.
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    delta = NULL,

    initialize = function(thresh, all_reached = FALSE) {
      assert_numeric(thresh, names = "unique", any.missing = FALSE)
      assert_flag(all_reached)

      super$initialize(settings = list(thresh = thresh, all_reached = all_reached))
      self$delta = set_names(rep(Inf, length(thresh)), names(thresh))
      self$terminated = FALSE
    },

    eval_before = function(pe) {
      invisible(self)
    },

    eval_after = function(pe) {
      thresh = self$settings$thresh
      measures = pe$measures[match(map_chr(pe$measures, "id"), names(thresh))]

      aggr = pe$bmr$aggregate(pe$measures)[, names(measures), with = FALSE]
      delta = pmap_dbl(list(aggr = aggr, thresh = thresh, measure = measures),
        function(aggr, thresh, measure) {
          if (measure$minimize) min(aggr - thresh) else max(thresh - aggr)
        }
      )
      names(delta) = names(thresh)
      self$delta = delta


      if (any(delta < 0)) {
        self$terminated = TRUE
      }

      invisible(self)
    }

  ),

  active = list(
    remaining = function() "<?>"
  )
)
