#' @title Logging object for objective function evaluations
#'
#' @description
#' Container around a [data.table::data.table()] which stores all performed
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
    #' Learner does not contain a model. Use `$learners()` to get learners with
    #' models.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learner = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learner
    },

    #' @description
    #' Retrieve list of trained [mlr3::Learner] objects of the i-th evaluation,
    #' by position or by unique hash `uhash`. `i` and `uhash` are mutually
    #' exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learners = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learners
    },

    #' @description
    #' Retrieve param values of the i-th evaluation, by position
    #' or by unique hash `uhash`. `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learner_param_vals = function(i = NULL, uhash = NULL) {
      self$learner(i = i, uhash = uhash)$param_set$values
    },

    #' @description
    #' Retrieve list of [mlr3::Prediction] objects of the i-th evaluation, by
    #' position or by unique hash `uhash`. `i` and `uhash` are mutually
    #' exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    predictions = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$predictions()
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
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      print(self$data[, setdiff(names(self$data), c("x_domain", "uhash")), with = FALSE], digits=2)
    }
  ),

  active = list(

    #' @field extended_archive ([data.table::data.table()])\cr
    #' Joins each performed function call of the [Objective] with the
    #' corresponding [mlr3::ResampleResult].
    extended_archive = function() {
      self$data[self$benchmark_result$resample_results, on = "uhash"]
    }
  )
)
