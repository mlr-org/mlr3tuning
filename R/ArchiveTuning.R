#' @title Logging object for objective function evaluations
#'
#' @description
#' Container around a [data.table::data.table] which stores all performed
#' function calls of the Objective and the associated [mlr3::BenchmarkResult].
#'
#' `$benchmark_result` stores a [mlr3::BenchmarkResult] which contains the
#' [mlr3::ResampleResult] of all performed function calls. The
#' [mlr3::BenchmarkResult] is connected to the [data.table::data.table] via the
#' `uhash` column.
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
