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
#' * `thresh` :: `numeric(1)`\cr
#'   Threshold that need to be reached.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized).
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    delta = NULL,

    initialize = function(thresh) {
      #FIXME: can we still name the measure? we then need to assert the validity of the name below
      assert_number(thresh)
      super$initialize(settings = list(thresh = thresh))
      self$delta = set_names(rep(Inf, length(thresh)), names(thresh))
    },

    eval_after = function(pe) {
      thresh = self$settings$thresh
      m = pe$measures[[1]]
      aggr = pe$aggregate()
      self$terminated =
        ( m$minimize && any(aggr[[m$id]] <= self$settings$thresh)) ||
        (!m$minimize && any(aggr[[m$id]] >= self$settings$thresh))
      invisible(self)
    }

  ),

  active = list(
    remaining = function() "<?>"
  )
)
