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
#' The performance evaluator is the major input for a [Tuner].
#'
#' Evaluations of HP configurations are performed in batches, and after a batch-eval, if the [Terminator] is positive,
#' an exception is raised. No further evaluations can be performed from this point on.
#'
#' @section Construction:
#' ```
#' pe = PerfEval$new(task, learner, resampling, measures, param_set, terminator, store_models = FALSE)
#' ```
#'
#' * `task` :: [mlr3::Task].
#'   See also [mlr3::mlr_sugar].
#' * `learner` :: [mlr3::Learner].
#'   See also [mlr3::mlr_sugar].
#' * `resampling` :: [mlr3::Resampling].
#'   See also [mlr3::mlr_sugar].
#' * `measures` :: list of [mlr3::Measure].
#'   See also [mlr3::mlr_sugar].
#' * `param_set` :: [paradox::ParamSet].
#' * `terminator` :: [Terminator].
#' * `store_models` :: `logical(1)`\cr
#'   Keep the fitted learner models? Passed down to [mlr3::benchmark()].
#' * `start_time` :: `POSIXct(1)`\cr
#'   Time the tuning / evaluations were started.
#'
#' @section Fields:
#' * `task` :: [mlr3::Task]\cr
#' * `learner` :: [mlr3::Learner]\cr
#' * `resampling` :: [mlr3::Resampling]\cr
#' * `measures` :: list of [mlr3::Measure]\cr
#' * `param_set` :: [paradox::ParamSet]\cr
#' * `terminator` :: [Terminator]\cr
#' * `store_models` :: `logical(1)`\cr
#' * `bmr` :: [mlr3::BenchmarkResult]\cr
#'   A benchmark result, container object for all performed [ResampleResult]s when evaluating HP configurations.
#' * `n_evals` :: `integer(1)`\cr
#'   Number of unique experiments stored in the container.
#'
#' @section Methods:
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> [data.table::data.table]\cr
#'   Evaluates all hyperparameter configurations in `dt` through resampling, where each configuration is a row, and columns are scalar parameters.
#'   Return a data.table with corresponding rows, where each column is an named measure.
#'   After a batch-eval the [Terminator] is checked, if it is positive, an exception of class `terminated_message` is raised.
#'   This function should be internally called by the tuner.
#' * `best(ties_method = "random")`\cr
#'   (`character(1)`) -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according to the first measure in `$measures`.
#'   `ties_method` can be "first", "last" or "random".
#' * `archive(unnest = TRUE)`
#'   `logical(1)` -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s
#'   `archive()` method. If `unnest` is `TRUE`, hyperparameter settings are stored in
#'   separate columns instead of inside a list column.
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
#' terminator = TerminatorEvals$new(5)
#' pe = PerfEval$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   param_set = param_set,
#'   terminator = terminator
#' )
#' pe$eval_batch(data.table(cp = c(0.05, 0.01), minsplit = c(5, 3)))
#' pe$archive()
PerfEval = R6Class("PerfEval",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    terminator = NULL,
    store_models = NULL,
    bmr = NULL,
    start_time = NULL,

    initialize = function(task, learner, resampling, measures, param_set, terminator, store_models = FALSE) {
      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = self$task)
      self$resampling = assert_resampling(resampling)
      self$measures = assert_measures(measures)
      self$param_set = assert_param_set(param_set)
      self$store_models = assert_flag(store_models)
      self$terminator = assert_r6(terminator, "Terminator")
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
      catf(str_indent("* Terminator:", format(self$terminator)))
      catf(str_indent("* store_models:", self$store_models))
      print(self$param_set)
      catf("Archive:")
      print(self$archive())
    },

    # evaluates all points in a design
    # possibly transforms the data before using the trafo from self$param set
    eval_batch = function(dt) {

      assert_data_table(dt, any.missing = FALSE, min.rows = 1, min.cols = 1)
      design = Design$new(self$param_set, dt, remove_dupl = FALSE)

      lg$info("Evaluating %i configurations", nrow(dt))

      # Not that pretty but enables the use of transpose from Design:
      if (self$param_set$has_trafo) {
        design$data = self$param_set$trafo(design$data)
      }

      learners = imap(design$transpose(), function(xt, i) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, xt)
        return(learner)
      })

      # eval via benchmark and check terminator
      bmr = benchmark(expand_grid(tasks = list(self$task), learners = learners,
        resamplings = list(self$resampling)), store_models = self$store_models)
      # store evalualted results
      if (is.null(self$bmr)) {
        self$bmr = bmr
      } else {
        self$bmr$combine(bmr)
      }
      # if the terminator is positive throw condition of class "terminated_message" that we can tryCatch
      if (self$terminator$is_terminated(self)) {
        stop(messageCondition("Termination criteria is reached", class = "terminated_message"))
      }

      # get aggregated measures in dt, return them
      mids = map_chr(self$measures, "id")
      return(bmr$aggregate(measures = self$measures)[, mids, with = FALSE])
    },

    archive = function(unnest = TRUE) {
      if (is.null(self$bmr)) {
        return(data.table())
      }
      dt = self$bmr$aggregate(measures = self$measures, params = TRUE)
      if (unnest) {
        dt = mlr3misc::unnest(dt, "params")
      }
      return(dt)
    },

    best = function(ties_method = "random") {
      # FIXME: we need tie handling?
      # measure = assert_measure(measure, learner = self$data$learner[[1L]])
      m = self$measures[[1L]]
      tab = self$bmr$aggregate(m, ids = FALSE)
      best = if (m$minimize) which_min else which_max
      tab$resample_result[[best(tab[[m$id]], ties_method = ties_method)]]
    }

  ),

  active = list(
    n_evals = function() length(self$bmr$hashes)
  )
)
