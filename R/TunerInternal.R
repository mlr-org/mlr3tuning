#' @title Internal Tuning
#' @name mlr_tuners_internal
#' @description
#' This is a special tuner which only conducts one round of learner-internal tuning.
#'
#' @family Tuner
#' @export
TunerInternal = R6Class("TunerInternal",
  inherit = Tuner,
  public = list(
    initialize = function() {
      super$initialize(
        id = "grid_search",
        param_set = ps(),
        param_classes = character(0),
        properties = c("dependencies", "single-crit", "multi-crit"),
        label = "Inner",
        man = "mlr3tuning::mlr_tuners_internal"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      .NotYetImplemented()
    }
  )
)


mlr_tuners$add("internal", TunerInternal)
