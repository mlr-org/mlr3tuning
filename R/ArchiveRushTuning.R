#' @title Class for Logging Evaluated Hyperparameter Configurations
#'
#' @description
#' The [ArchiveRushTuning] stores all evaluated hyperparameter configurations and performance scores.
#'
#' @details
#' The [ArchiveRushTuning] is a container around a [data.table::data.table()].
#' Each row corresponds to a single evaluation of a hyperparameter configuration.
#' See the section on Data Structure for more information.
#' The archive stores additionally a [mlr3::BenchmarkResult] (`$benchmark_result`) that records the resampling experiments.
#' Each experiment corresponds to to a single evaluation of a hyperparameter configuration.
#' The table (`$data`) and the benchmark result (`$benchmark_result`) are linked by the `uhash` column.
#' If the archive is passed to `as.data.table()`, both are joined automatically.
#'
#' @section Data Structure:
#'
#' The table (`$data`) has the following columns:
#'
#' * One column for each hyperparameter of the search space (`$search_space`).
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
#' * `uhash` (`character(1)`)\cr
#'     Connects each hyperparameter configuration to the resampling experiment stored in the [mlr3::BenchmarkResult].
#'
#' @section Analysis:
#' For analyzing the tuning results, it is recommended to pass the [ArchiveTuning] to `as.data.table()`.
#' The returned data table is joined with the benchmark result which adds the [mlr3::ResampleResult] for each hyperparameter evaluation.
#'
#' The archive provides various getters (e.g. `$learners()`) to ease the access.
#' All getters extract by position (`i`) or unique hash (`uhash`).
#' For a complete list of all getters see the methods section.
#'
#' The benchmark result (`$benchmark_result`) allows to score the hyperparameter configurations again on a different measure.
#' Alternatively, measures can be supplied to `as.data.table()`.
#'
#' The \CRANpkg{mlr3viz} package provides visualizations for tuning results.
#'
#' @section S3 Methods:
#' * `as.data.table.ArchiveTuning(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`\cr
#' Returns a tabular view of all evaluated hyperparameter configurations.\cr
#' [ArchiveTuning] -> [data.table::data.table()]\cr
#'     * `x` ([ArchiveTuning])
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
#'
#' @export
ArchiveRushTuning = R6Class("ArchiveRushTuning",
  inherit = bbotk::ArchiveRush,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   If `TRUE` (default), hyperparameter configurations are check for validity.
    initialize = function(search_space, codomain, rush) {
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
      print(as.data.table(self, unnest = NULL, exclude_columns = c("x_domain", "uhash", "timestamp", "runtime_learners", "resample_result")), digits = 2)
    },

    #' @description
    #' Copy the data from rush to a local [data.table::data.table()].
    freeze = function() {
      # copy benchmark result to cache
      self$benchmark_result
      super$freeze()
    }
  ),

  active = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Benchmark result.
    benchmark_result = function() {
      # return cached benchmark result when archive is frozen
      if (is.null(self$rush)) return(private$.benchmark_result)

      # cache benchmark result
      if (self$rush$n_finished_tasks > private$.benchmark_result$n_resample_results) {
        bmrs = map(self$data$resample_result, as_benchmark_result)
        init = BenchmarkResult$new()
        private$.benchmark_result = Reduce(function(lhs, rhs) lhs$combine(rhs), bmrs, init = init)
      }
      private$.benchmark_result
    }
  ),

  private = list(
    .benchmark_result = NULL
  )
)

#' @export
as.data.table.ArchiveRushTuning = function(x, ..., unnest = "x_domain", exclude_columns = NULL, measures = NULL) {
  if (nrow(x$data) == 0) return(data.table())
  # default values for unnest and exclude_columns might be not present in archive
  if ("x_domain" %nin% names(x$data)) unnest = setdiff(unnest, "x_domain")

  assert_subset(unnest, names(x$data))
  cols_y_extra = NULL

  # unnest data
  tab = unnest(copy(x$data), unnest, prefix = "{col}_")

  # add extra measures
  if (!is.null(measures) && !is.null(x$data$resample_result)) {
    measures = assert_measures(as_measures(measures), learner = x$learners(1)[[1]], task = x$resample_result(1)$task)
    cols_y_extra = map_chr(measures, "id")
    scores = map_dtr(x$data$resample_result, function(rr) as.data.table(as.list(rr$aggregate(measures))))
    tab = cbind(tab, scores)
  }

  cols_x_domain =  if ("x_domain" %in% unnest) {
    # get all ids of x_domain
    # trafo could add unknown ids
    x_domain_ids = paste0("x_domain_", unique(unlist(map(x$data$x_domain, names))))
    setdiff(x_domain_ids, exclude_columns)
  } else NULL

  setcolorder(tab, c(x$cols_x, x$cols_y, cols_y_extra, cols_x_domain, "runtime_learners", "timestamp_xs", "timestamp_ys"))
  assert_subset(exclude_columns, names(tab))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}
