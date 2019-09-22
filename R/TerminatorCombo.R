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
#' TerminatorCombo$new(terminators = list(TerminatorNone$new()))
#' term("combo")
#' ```
#' * `terminators` :: `list()`\cr
#'   List of objects of class [Terminator].
#'
#' @section Parameters:
#' * `any` :: `logical(1)`\cr
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
    terminators = NULL,

    initialize = function(terminators = list(TerminatorNone$new())) {
      self$terminators = assert_list(terminators, types = "Terminator", min.len = 1L)
      ps = ParamSet$new(list(ParamLgl$new("any", default = TRUE, tags = "required")))
      ps$values = list(any = TRUE)
      super$initialize(param_set = ps)
    },

    is_terminated = function(instance) {
      g = if (self$param_set$values$any) any else all
      g(map_lgl(self$terminators, function(t) t$is_terminated(instance)))
    }
  )
)

mlr_terminators$add("combo", TerminatorCombo)
