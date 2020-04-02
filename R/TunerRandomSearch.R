#' @title TunerRandomSearch
#'
#' @name mlr_tuners_random_search
#' @include Tuner.R
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
#' @templateVar id random_search
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' * `batch_size` (`integer(1)`)\cr
#'   Maximum number of configurations to try in a batch.
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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
    .tune = function(instance) {
      batch_size = self$param_set$values$batch_size
      repeat { # iterate until we have an exception from eval_batch
        design = generate_design_random(instance$param_set, batch_size)
        instance$objective$eval_batch(design$data)
      }
    }
  )
)

mlr_tuners$add("random_search", TunerRandomSearch)
