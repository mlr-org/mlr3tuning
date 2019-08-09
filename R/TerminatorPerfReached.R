#' @title Terminator that stops when a peformance level has been reached
#'
#' @include Terminator.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#'
#' @description
#' Class to terminate the tuning after a performance level has been hit.
#'
#' @section Construction:
#' ```
#' t = TerminatorPerfReached$new(level)
#' ```
#' * `level` :: `numeric(1)`\cr
#'   Level that needs to be reached.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized).
#'
#' @family Terminator
#' @export
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(
    initialize = function(level) {
      #FIXME: can we still name the measure? we then need to assert the validity of the name below
      assert_number(level)
      super$initialize(settings = list(level = level))
    },

    eval_after = function(pe) {
      level = self$settings$level
      m = pe$measures[[1]]
      aggr = pe$archive()
      self$terminated =
        ( m$minimize && any(aggr[[m$id]] <= self$settings$level)) ||
        (!m$minimize && any(aggr[[m$id]] >= self$settings$level))
      invisible(self)
    }

  ),

  active = list(
    remaining = function() "<?>"
  )
)
