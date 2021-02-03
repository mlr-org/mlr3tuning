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
    benchmark_result = NULL,

    #' @description
    #' Retrieve [mlr3::Learner] of the i-th evaluation, by position
    #' or by unique hash `uhash`. `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learner = function(i = NULL, uhash = NULL) {
      self$benchmark_result$resample_result(i = i, uhash = uhash)$learner
    },

    #' @description
    #' Retrieve [mlr3::ResampleResult] of the i-th evaluation, by position
    #' or by unique hash `uhash`. `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    resample_result = function(i = NULL, uhash = NULL) {
      self$benchmark_result$resample_result(i = i, uhash = uhash)
    },

    #' @description
    #' Joins archive data with [mlr3::ResampleResult] stored in [mlr3::BenchmarkResult].
    join = function() {
      setDT(self$data)[instance$archive$benchmark_result$resample_results, on = "uhash"] 
    }
  ),
)
