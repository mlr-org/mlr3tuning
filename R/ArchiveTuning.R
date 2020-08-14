#' @title Logging object for objective function evaluations
#'
#' @description
#' Container around a [data.table::data.table] which stores all performed
#' function calls of the Objective and the associated [mlr3::BenchmarkResult].
#'
#' `$benchmark_result` stores a [mlr3::BenchmarkResult] which contains the
#' [mlr3::ResampleResult] of all performaned function calls. The
#' [mlr3::BenchmarkResult] is connected to the [data.table::data.table] via the
#' `uhash` column.
#'
#' @section Technical details:
#'
#' The data is stored in a private `.data` field that contains a
#' [data.table::data.table] which logs all performed function calls of the [ObjectiveTuning].
#' This [data.table::data.table] is accessed with the public `$data()` method. New
#' values can be added with the `$add_evals()` method. This however is usually
#' done through the evaluation of the [TuningInstanceSingleCrit] or
#' [TuningInstanceMultiCrit] by the [Tuner].
#'
#' @template param_codomain
#' @template param_search_space
#' @template param_xdt
#' @template param_ydt
#' @template param_check_values
#'
#' @export
ArchiveTuning = R6Class("ArchiveTuning",
  inherit = Archive,

  public = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Stores benchmark result.
    benchmark_result = NULL
  )
)
