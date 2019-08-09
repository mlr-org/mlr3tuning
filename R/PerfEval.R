#' @title PerfEval Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Implements a performance evaluator for tuning. This class encodes the black box objective function,
#' that a generic tuner has to optimize. It allows the basic operations of querying the objective
#' at design points (see `eval_batch`), storing the evaluated point in an internal archive
#' and querying the archive (see `archive`).
#'
#' The performance evaluator is one of the major inputs for constructing a [Tuner].
#'
#' @section Construction:
#' ```
#' pe = PerfEval$new(task, learner, resampling, measures, param_set,
#'   store_models = FALSE)
#'```
#'
#' * `task` :: [mlr3::Task].
#' * `learner` :: [mlr3::Learner].
#' * `resampling` :: [mlr3::Resampling].
#' * `measures` :: list of [mlr3::Measure].
#' * `param_set` :: [paradox::ParamSet].
#' * `store_models` :: `logical(1)`\cr
#'   Keep the fitted learner models? Passed down to [mlr3::benchmark()].
#'
#' @section Fields:
#' * `task` :: [mlr3::Task]\cr
#'   Stored task.
#' * `learner` :: [mlr3::Learner]\cr
#'   Stored learner.
#' * `resampling` :: [mlr3::Resampling]\cr
#'   Stored resampling
#' * `measures` :: list of [mlr3::Measure]\cr
#'   Stored measures.
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Stored parameter set.
#' * `bmr` :: [mlr3::BenchmarkResult]\cr
#'   A benchmark result, which is used as data storage.
#' * `n_evals` :: `integer(1)`\cr
#'   Number of unique experiments stored in the container.
#'
#' @section Methods:
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> [data.table::data.table]\cr
#'   Evaluates all hyperparameter configurations in `dt` through resampling, where each configuration is a row, and columns are scalar parameters.
#'   Return a data.table with corresponding rows, where each column is an named measure.
#' * `best(ties_method = "random")`\cr
#'   (`character(1)`) -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according to the first measure in `$measures`.
#'   `ties_method` can be "first", "last" or "random".
#' * `archive(unnest = TRUE)`
#'   `logical(1)` -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s
#'   `archive()` method. If `unnest` is `TRUE`, hyperparameter settings are stored in
#'   separate columns instead of inside a list column
#'
#' @family PerfEval
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
#' # Object required to define the performance evaluator:
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("holdout")
#' measures = mlr_measures$mget("classif.ce")
#' param_set = ParamSet$new(params = list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   ParamInt$new("minsplit", lower = 1, upper = 10)))
#'
#' pe = PerfEval$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   param_set = param_set
#' )
#'
#' pe$eval_batch(data.table(cp = c(0.05, 0.01), minsplit = c(5, 3)))
#' pe$best()
PerfEval = R6Class("PerfEval",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    store_models = NULL,
    bmr = NULL,

    initialize = function(task, learner, resampling, measures, param_set, store_models = FALSE) {
      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = self$task)
      self$resampling = assert_resampling(resampling)
      self$measures = assert_measures(measures)
      self$param_set = assert_param_set(param_set)
      self$store_models = assert_flag(store_models)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* Task:", format(self$task)))
      catf(str_indent("* Learner:", format(self$learner)))
      catf(str_indent("* Measures:", map_chr(self$measures, "id")))
      catf(str_indent("* Resampling:", format(self$resampling)))
      print(self$param_set)
      catf("Evals:")
      if (!is.null(self$bmr))
        print(self$bmr)
      else
        catf("[None yet]")
    },

    # evaluates all points in a design
    # possibly transforms the data before using the trafo from self$param set
    eval_batch = function(dt) {
      assert_data_table(dt, any.missing = FALSE, min.rows = 1, min.cols = 1)
      design = Design$new(self$param_set, dt, remove_dupl = FALSE)

      # Not that pretty but enables the use of transpose from Design:
      if (self$param_set$has_trafo) {
        design$data = self$param_set$trafo(design$data)
      }

      learners = imap(design$transpose(), function(xt, i) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, xt)
        return(learner)
      })

      bmr = benchmark(expand_grid(tasks = list(self$task), learners = learners,
        resamplings = list(self$resampling)), store_models = self$store_models)

      if (is.null(self$bmr)) {
        self$bmr = bmr
      } else {
        self$bmr$combine(bmr)
      }
      # get aggregated measures in dt, return them
      mids = map_chr(self$measures, "id")
      return(bmr$aggregate(measures = self$measures)[, mids, with = FALSE])
    },

    archive = function(unnest = TRUE) {
      if (is.null(self$bmr)) {
        stopf("No tuning conducted yet.")
      }
      dt = self$bmr$aggregate(measures = self$measures, params = TRUE)
      if (unnest) {
        dt = mlr3misc::unnest(dt, "params")
      }
      return(dt)
    },

    best = function(ties_method = "random") {
      #FIXME: we need tie handling?
      # measure = assert_measure(measure, learner = self$data$learner[[1L]])
      m = self$measures[[1L]]
      tab = self$bmr$aggregate(m, ids = FALSE)
      best = if (m$minimize) which_min else which_max
      tab$resample_result[[best(tab[[m$id]], ties_method = ties_method)]]
    }

  ),

  active = list(
    n_evals = function() length(self$bmr$hashes) # FIXME: maybe at some point BMR container has AB "size"?
  )
)
