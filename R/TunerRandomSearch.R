#' @title TunerRandomSearch
#'
#' @aliases mlr_tuners_random_search
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for random search tuning.
#'
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @section Construction:
#' ```
#' TunerRandomSearch$new(batch_size = 1L)
#' tnr("random_search")
#' ```
#'
#' @section Parameters:
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("batch_size", lower = 1L, tags = "required")
      ))
      ps$values = list(batch_size = 1L)

      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies"
      )
    }
  ),

  private = list(
    tune_internal = function(instance) {
      batch_size = self$param_set$values$batch_size
      repeat { # iterate until we have an exception from eval_batch
        design = generate_design_random(instance$param_set, batch_size)
        instance$eval_batch(design$data)
      }
    }
  )
)

mlr_tuners$add("random_search", TunerRandomSearch)
