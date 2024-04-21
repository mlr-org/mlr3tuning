#' @title Class for Asynchronous Tuning Algorithms
#'
#' @include mlr_tuners.R
#'
#' @description
#' The [TunerAsync] implements the asynchronous optimization algorithm.
#'
#' @details
#' [TunerAsync] is an abstract base class that implements the base functionality each asynchronous tuner must provide.
#'
#' @inheritSection Tuner Resources
#'
#' @template param_id
#' @template param_param_set
#' @template param_param_classes
#' @template param_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @export
TunerAsync = R6Class("TunerAsync",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "async_tuner",
      param_set,
      param_classes,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
      ) {
      super$initialize(
        id = id,
        param_set = param_set,
        param_classes = param_classes,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceAsyncSingleCrit] or [TuningInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsyncTuning] that resides in the [TuningInstanceAsyncSingleCrit]/[TuningInstanceAsyncMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceAsyncSingleCrit] | [TuningInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      assert_multi_class(inst, c("TuningInstanceAsyncSingleCrit", "TuningInstanceAsyncMultiCrit"))

      # start workers
      start_async_optimize(inst, self, private)

      # print logs and check for termination
      wait_for_async_optimize(inst, self, private)

      # assign and print results
      finish_async_optimize(inst, self, private)
    }
  )
)

