#' @title Frozen Rush Data Storage
#'
#' @description
#' Freezes the Redis data base of an [ArchiveAsyncTuning] to a  `data.table::data.table()`.
#' No further points can be added to the archive but the data can be accessed and analyzed.
#' Useful when the Redis data base is not permanently available.
#' Use the callback [mlr3tuning.async_freeze_archive] to freeze the archive after the optimization has finished.
#'
#' @section S3 Methods:
#' * `as.data.table(archive)`\cr
#'   [ArchiveAsyncTuningFrozen] -> [data.table::data.table()]\cr
#'   Returns a tabular view of all performed function calls of the Objective.
#'   The `x_domain` column is unnested to separate columns.
#'
#' @export
ArchiveAsyncTuningFrozen = R6Class("ArchiveAsyncTuningFrozen",
  inherit = bbotk::ArchiveAsyncFrozen,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([ArchiveAsyncTuning])\cr
    #' The archive to freeze.
    initialize = function(archive) {
      private$.benchmark_result = archive$benchmark_result
      private$.internal_search_space = archive$internal_search_space
      super$initialize(archive)
    },

    #' @description
    #' Retrieve [mlr3::Learner] of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #' Learner does not contain a model. Use `$learners()` to get learners with models.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    learner = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learner
    },

    #' @description
    #' Retrieve list of trained [mlr3::Learner] objects of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    learners = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learners
    },

    #' @description
    #' Retrieve param values of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    learner_param_vals = function(i = NULL, uhash = NULL) {
      self$learner(i = i, uhash = uhash)$param_set$values
    },

    #' @description
    #' Retrieve list of [mlr3::Prediction] objects of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    predictions = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$predictions()
    },

    #' @description
    #' Retrieve [mlr3::ResampleResult] of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    resample_result = function(i = NULL, uhash = NULL) {
      self$benchmark_result$resample_result(i = i, uhash = uhash)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf("%s with %i evaluations", format(self), self$n_evals)
      print(as.data.table(self, unnest = NULL, exclude_columns = c(
        "x_domain",
        "timestamp_xs",
        "timestamp_ys",
        "runtime_learners",
        "resample_result",
        "worker_id",
        "keys",
        "pid",
        "state")), digits = 2)
    }
  ),

  active = list(
    #' @field internal_search_space ([paradox::ParamSet])\cr
    #'   The search space containing those parameters that are internally optimized by the [`mlr3::Learner`].
    internal_search_space = function(rhs) {
      assert_ro_binding(rhs)
      private$.internal_search_space
    },

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Benchmark result.
    benchmark_result = function() {
      private$.benchmark_result
    }
  ),

  private = list(
    .internal_search_space = NULL,
    .benchmark_result = NULL
  )
)

#' @export
as.data.table.ArchiveAsyncTuningFrozen = function(x, ..., unnest = "internal_tuned_values", exclude_columns = NULL, measures = NULL) {
  data = copy(x$data)
  if (!nrow(data)) return(data.table())

  # unnest columns
  cols = intersect(unnest, names(data))
  tab = unnest(data, cols, prefix = "{col}_")

  # add extra measures
  cols_y_extra = NULL
  if (!is.null(measures) && !is.null(tab$resample_result)) {
    measures = assert_measures(as_measures(measures), learner = x$learners(1)[[1]], task = x$resample_result(1)$task)
    cols_y_extra = map_chr(measures, "id")
    scores = map_dtr(x$data$resample_result, function(rr) as.data.table(as.list(rr$aggregate(measures))))
    tab = cbind(tab, scores)
  }

  cols_x_domain =  if ("x_domain" %in% cols) {
    # get all ids of x_domain
    # trafo could add unknown ids
    x_domain_ids = paste0("x_domain_", unique(unlist(map(x$data$x_domain, names))))
    setdiff(x_domain_ids, exclude_columns)
  }

  cols_internal_tuned_values =  if ("internal_tuned_values" %in% cols) {
    internal_tuned_values_ids = paste0("internal_tuned_values_", unique(unlist(map(x$data$internal_tuned_values, names))))
    setdiff(internal_tuned_values_ids, exclude_columns)
  }

  setcolorder(tab, c(x$cols_x, x$cols_y, cols_y_extra, cols_internal_tuned_values, cols_x_domain, "runtime_learners", "timestamp_xs", "timestamp_ys"))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}
