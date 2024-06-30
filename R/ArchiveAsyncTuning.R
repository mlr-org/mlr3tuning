#' @title Rush Data Storage
#'
#' @description
#' The `ArchiveAsyncTuning`` stores all evaluated hyperparameter configurations and performance scores in a [rush::Rush] database.
#'
#' @details
#' The [ArchiveAsyncTuning] is a connector to a [rush::Rush] database.
#'
#' @section Data Structure:
#'
#' The table (`$data`) has the following columns:
#'
#' * One column for each hyperparameter of the search space (`$search_space`).
#' * One (list-)column for the `internal_tuned_values`
#' * One column for each performance measure (`$codomain`).
#' * `x_domain` (`list()`)\cr
#'     Lists of (transformed) hyperparameter values that are passed to the learner.
#' * `runtime_learners` (`numeric(1)`)\cr
#'     Sum of training and predict times logged in learners per [mlr3::ResampleResult] / evaluation.
#'     This does not include potential overhead time.
#' * `timestamp` (`POSIXct`)\cr
#'     Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'     Hyperparameters are evaluated in batches.
#'     Each batch has a unique batch number.
#'
#' @section Analysis:
#' For analyzing the tuning results, it is recommended to pass the [ArchiveAsyncTuning] to `as.data.table()`.
#' The returned data table contains the [mlr3::ResampleResult] for each hyperparameter evaluation.
#'
#' @section S3 Methods:
#' * `as.data.table.ArchiveTuning(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`\cr
#' Returns a tabular view of all evaluated hyperparameter configurations.\cr
#' [ArchiveAsyncTuning] -> [data.table::data.table()]\cr
#'     * `x` ([ArchiveAsyncTuning])
#'     * `unnest` (`character()`)\cr
#'       Transforms list columns to separate columns. Set to `NULL` if no column should be unnested.
#'     * `exclude_columns` (`character()`)\cr
#'       Exclude columns from table. Set to `NULL` if no column should be excluded.
#'     * `measures` (List of [mlr3::Measure])\cr
#'       Score hyperparameter configurations on additional measures.
#'
#' @template param_search_space
#' @template param_codomain
#' @template param_rush
#' @template param_internal_search_space
#'
#' @export
ArchiveAsyncTuning = R6Class("ArchiveAsyncTuning",
  inherit = bbotk::ArchiveAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   If `TRUE` (default), hyperparameter configurations are check for validity.
    initialize = function(
      search_space,
      codomain,
      rush,
      internal_search_space = NULL
      ) {
      init_internal_search_space_archive(self, private, super, search_space, internal_search_space)

      super$initialize(
        search_space = search_space,
        codomain = codomain,
        rush = rush)
      private$.benchmark_result = BenchmarkResult$new()
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
      # cache benchmark result
      if (self$rush$n_finished_tasks > private$.benchmark_result$n_resample_results) {
        bmrs = map(self$finished_data$resample_result, as_benchmark_result)
        init = BenchmarkResult$new()
        private$.benchmark_result = Reduce(function(lhs, rhs) lhs$combine(rhs), bmrs, init = init)
      }
      private$.benchmark_result
    }
  ),

  private = list(
    .internal_search_space = NULL,
    .benchmark_result = NULL
  )
)

#' @export
as.data.table.ArchiveAsyncTuning = function(x, ..., unnest = "x_domain", exclude_columns = NULL, measures = NULL) {
  data = x$data_with_state()
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

  setcolorder(tab, c(x$cols_x, if (length(x$internal_search_space$ids())) "internal_tuned_values", x$cols_y, cols_y_extra, cols_x_domain,
      "runtime_learners", "timestamp_xs", "timestamp_ys"))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}
