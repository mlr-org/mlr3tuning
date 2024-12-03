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
ArchiveAsyncTuningFrozen = R6Class("ArchiveAsyncTuning",
  inherit = bbotk::ArchiveAsync,
  public = list(

    frozen_data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   If `TRUE` (default), hyperparameter configurations are check for validity.
    initialize = function(archive) {
      self$frozen_data = copy(archive$data)
      private$.benchmark_result = archive$benchmark_result
      private$.internal_search_space = archive$internal_search_space
      self$search_space = archive$search_space
      self$codomain = archive$codomain


    },

    #' @description
    #' Push queued points to the archive.
    #'
    #' @param xss (list of named `list()`)\cr
    #' List of named lists of point values.
    push_points = function(xss) {
      stop("Archive is frozen")
    },

    #' @description
    #' Pop a point from the queue.
    pop_point = function() {
      stop("Archive is frozen")
    },

    #' @description
    #' Push running point to the archive.
    #'
    #' @param xs (named `list`)\cr
    #' Named list of point values.
    #' @param extra (`list()`)\cr
    #' Named list of additional information.
    push_running_point = function(xs, extra = NULL) {
      stop("Archive is frozen")
    },

    #' @description
    #' Push result to the archive.
    #'
    #' @param key (`character()`)\cr
    #' Key of the point.
    #' @param ys (`list()`)\cr
    #' Named list of results.
    #' @param x_domain (`list()`)\cr
    #' Named list of transformed point values.
    #' @param extra (`list()`)\cr
    #' Named list of additional information.
    push_result = function(key, ys, x_domain, extra = NULL) {
      stop("Archive is frozen")
    },

    #' @description
    #' Push failed point to the archive.
    #'
    #' @param key (`character()`)\cr
    #' Key of the point.
    #' @param message (`character()`)\cr
    #' Error message.
    push_failed_point = function(key, message) {
      self$rush$push_failed(key, list(list(message = message)))
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
      print(as.data.table(self$frozen_data, unnest = NULL, exclude_columns = c(
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
    },

    #' @field data ([data.table::data.table])\cr
    #' Data table with all finished points.
    data = function(rhs) {
      assert_ro_binding(rhs)
      self$frozen_data
    },

    #' @field queued_data ([data.table::data.table])\cr
    #' Data table with all queued points.
    queued_data = function() {
      self$frozen_data[state == "queued"]
    },

    #' @field running_data ([data.table::data.table])\cr
    #' Data table with all running points.
    running_data = function() {
      self$frozen_data[state == "running"]
    },

    #' @field finished_data ([data.table::data.table])\cr
    #' Data table with all finished points.
    finished_data = function() {
      self$frozen_data[state == "finished"]
    },

    #' @field failed_data ([data.table::data.table])\cr
    #' Data table with all failed points.
    failed_data = function() {
      self$frozen_data[state == "failed"]
    },

    #' @field n_queued (`integer(1)`)\cr
    #' Number of queued points.
    n_queued = function() {
      nrow(self$queued_data)
    },

    #' @field n_running (`integer(1)`)\cr
    #' Number of running points.
    n_running = function() {
      nrow(self$running_data)
    },

    #' @field n_finished (`integer(1)`)\cr
    #' Number of finished points.
    n_finished = function() {
      nrow(self$finished_data)
    },

    #' @field n_failed (`integer(1)`)\cr
    #' Number of failed points.
    n_failed = function() {
      nrow(self$failed_data)
    },

    #' @field n_evals (`integer(1)`)\cr
    #' Number of evaluations stored in the archive.
    n_evals = function() {
      nrow(self$finished_data) + nrow(self$failed_data)
    }
  ),

  private = list(
    .internal_search_space = NULL,
    .benchmark_result = NULL
  )
)

#' @export
as.data.table.ArchiveAsyncTuning = function(x, ..., unnest = "internal_tuned_values", exclude_columns = NULL, measures = NULL) {
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

  cols_internal_tuned_values =  if ("internal_tuned_values" %in% cols) {
    internal_tuned_values_ids = paste0("internal_tuned_values_", unique(unlist(map(x$data$internal_tuned_values, names))))
    setdiff(internal_tuned_values_ids, exclude_columns)
  }

  setcolorder(tab, c(x$cols_x, x$cols_y, cols_y_extra, cols_internal_tuned_values, cols_x_domain, "runtime_learners", "timestamp_xs", "timestamp_ys"))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}
