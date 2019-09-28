#' @title TunerDesignPoints
#'
#' @aliases mlr_tuners_design_points
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for tuning w.r.t. fixed design points.
#'
#' We simply search over a set of points fully specified by the user.
#' The points in the design are evaluated in order as given.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @section Construction:
#' ```
#' TunerDesignPoints$new()
#' tnr("design_points")
#' ```
#'
#' @section Parameters:
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerDesignPoints = R6Class("TunerDesignPoints",
  inherit = Tuner,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("batch_size", lower = 1L, tags = "required"),
        ParamUty$new("design", tags = "required")
      ))
      ps$values = list(batch_size = 1L, design = NULL)
      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      pv = self$param_set$values
      if (is.null(pv$design))
        stopf("Please set design datatable!")
      d = Design$new(pv$design, param_set = instance$param_set, remove_dupl = FALSE) # does assert for us
      ch = chunk_vector(seq_row(d$data), chunk_size = pv$batch_size, shuffle = FALSE)
      for (inds in ch) {
        instance$eval_batch(d$data[inds, ])
      }
    }
  )
)

mlr_tuners$add("design_points", TunerDesignPoints)

