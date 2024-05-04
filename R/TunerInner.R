#' @title Inner Tuning
#' @name mlr_tuners_inner
#' @description
#' This is a special tuner which only conducts one round of learner-internal ("inner") tuning.
#'
#' @family Tuner
#' @export
TunerInner = R6Class("TunerInner",
  inherits = Tuner,
  public = list(
    initialize = function() {
      super$initialize(
        id = "grid_search",
        param_set = ps(),
        param_classes = character(0),
        properties = c("dependencies", "single-crit", "multi-crit"),
        label = "Inner",
        man = "mlr3tuning::mlr_tuners_inner"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      .NotYetImplemented()
    }
  )
)


mlr_tuners$add("inner", TunerInner)
