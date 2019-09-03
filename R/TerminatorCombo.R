#' @title Combine Terminators
#'
#' @aliases mlr_terminators_combo
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' This class takes multiple [Terminator]s and terminates as soon as one or all of the included terminators are positive.
#'
#' @section Construction:
#' ```
#' TerminatorCombo$new(terminators = list(TerminatorNone$new()), any = TRUE)
#' term("combo")
#' ```
#' * `terminators` :: `list()`\cr
#'   List of objects of class [Terminator].
#'
#' * `any` :: `logical(1)`\cr
#'   Terminate iff any included terminator is positive? (not all).
#'   Stored in the parameter set `$param_set`.
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorCombo$new(list(
#'   TerminatorModelTime$new(),
#'   TerminatorEvals$new()
#' ))
#'
#' term("combo",
#'   list(term("model_time", secs = 60), term("evals", n_evals = 10)),
#'   any = FALSE
#' )
TerminatorCombo = R6Class("TerminatorCombo",
  inherit = Terminator,

  public = list(
    terminators = NULL,

    initialize = function(terminators = list(TerminatorNone$new()), any = TRUE) {
      self$terminators = assert_list(terminators, types = "Terminator", min.len = 1L)
      super$initialize(
        ParamSet$new(list(ParamLgl$new("any", default = TRUE, tags = "required"))),
        list(any = assert_flag(any))
      )
    },

    is_terminated = function(inst) {
      g = if (self$param_set$values$any) any else all
      g(map_lgl(self$terminators, function(t) t$is_terminated(inst)))
    }
  )
)

mlr_terminators$add("combo", TerminatorCombo)
