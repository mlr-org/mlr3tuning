#' @title Combine Terminators
#'
#' @name mlr_terminators_combo
#' @include Terminator.R
#'
#' @description
#' This class takes multiple [Terminator]s and terminates as soon as one or all of the included terminators are positive.
#'
#' @templateVar id combo
#' @template section_dictionary_terminator
#'
#' @section Parameters:
#' * `any` (`logical(1)`)\cr
#'   Terminate iff any included terminator is positive? (not all), default is `TRUE.
#'
#' @family Terminator
#' @export
#' @examples
#' term("combo",
#'   list(term("model_time", secs = 60), term("evals", n_evals = 10)),
#'   any = FALSE
#' )
TerminatorCombo = R6Class("TerminatorCombo",
  inherit = Terminator,

  public = list(
    #' @field terminators (`list()`)\cr
    #'   List of objects of class [Terminator].
    terminators = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param terminators (`list()`)\cr
    #'   List of objects of class [Terminator].
    initialize = function(terminators = list(TerminatorNone$new())) {
      self$terminators = assert_list(terminators, types = "Terminator", min.len = 1L)
      ps = ParamSet$new(list(ParamLgl$new("any", default = TRUE, tags = "required")))
      ps$values = list(any = TRUE)
      super$initialize(param_set = ps)
    },

    #' @description
    #' Is `TRUE` iff the termination criterion is positive, and `FALSE` otherwise.
    #'
    #' @param instance ([TuningInstance]).
    #'
    #' @return `logical(1)`.
    is_terminated = function(instance) {
      g = if (self$param_set$values$any) any else all
      g(map_lgl(self$terminators, function(t) t$is_terminated(instance)))
    }
  )
)

mlr_terminators$add("combo", TerminatorCombo)
