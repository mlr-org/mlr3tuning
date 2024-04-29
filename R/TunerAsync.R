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
    #' Performs the tuning on a [TuningInstanceAsyncSingleCrit] or [TuningInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsyncTuning] that resides in the [TuningInstanceAsyncSingleCrit]/[TuningInstanceAsyncMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceAsyncSingleCrit] | [TuningInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      assert_tuning_instance_async(inst)
      optimize_async_default(inst, self)
    }
  )
)

