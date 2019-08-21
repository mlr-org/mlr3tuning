#' @title TuningInstance Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Specifies a general tuning scenario, including performance evaluator and archive for Tuners to
#' act upon. This class encodes the black box objective function,
#' that a [Tuner] has to optimize. It allows the basic operations of querying the objective
#' at design points (see `$eval_batch()`), storing the evaluated point in an internal archive
#' and querying the archive (see `$archive()`).
#'
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' After a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#'
#' @section Construction:
#' ```
#' pe = TuningInstance$new(task, learner, resampling, measures, param_set, terminator, bm_args = list())
#' ```
#' This defines the resampled performance of a learner on a task, a feasibility region
#' for the parameters the tuner is supposed to optimize, and a termination criterion.
#'
#' * `task` :: [mlr3::Task] | [mlr3::mlr_sugar].
#' * `learner` :: [mlr3::Learner] | [mlr3::mlr_sugar].
#' * `resampling` :: [mlr3::Resampling] | [mlr3::mlr_sugar].
#' * `measures` :: list of [mlr3::Measure] | [mlr3::mlr_sugar].
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Hyperparameter search space.
#' * `terminator` :: [Terminator].
#' * `bm_args` :: named `list()`\cr
#'   Further args for [mlr::benchmark()].
#'
#' @section Fields:
#' * `task` :: [mlr3::Task].
#' * `learner` :: [mlr3::Learner].
#' * `resampling` :: [mlr3::Resampling].
#' * `measures` :: list of [mlr3::Measure].
#' * `param_set` :: [paradox::ParamSet].
#' * `terminator` :: [Terminator].
#' * `bmr` :: [mlr3::BenchmarkResult]\cr
#'   A benchmark result, container object for all performed [mlr3::ResampleResult]s when evaluating hyperparameter configurations.
#' * `n_evals` :: `integer(1)`\cr
#'   Number of configuration evaluations stored in the container.
#' * `start_time` :: `POSIXct(1)`\cr
#'   Time the tuning / evaluations were started.
#'   This is set in the beginning of `tune` of [Tuner].
#'
#' @section Methods:
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> [data.table::data.table()]\cr
#'   Evaluates all hyperparameter configurations in `dt` through resampling, where each configuration is a row, and columns are scalar parameters.
#'   Returns a `data.table()` with corresponding rows, where each column is a named measure.
#'   After a batch-eval the [Terminator] is checked, if it is positive, an exception of class `terminated_message` is raised.
#'   This function should be internally called by the tuner.
#'
#' * `best(measure = NULL, ties_method = "random")`\cr
#'   ([mlr3::Measure] | [mlr3::mlr_sugar], `character(1)`) -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according `measure` (default is the first measure in `$measures`).
#'   `ties_method` can be "first", "last" or "random" (c.f. [mlr3misc::which_max()]).
#'
#' * `archive(unnest = TRUE)`
#'   `logical(1)` -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s
#'   `$aggregate()` method. If `unnest` is `TRUE`, hyperparameter settings are stored in
#'   separate columns instead of inside a list column.
#'
#' @family TuningInstance
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
#' # Object required to define the performance evaluator:
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measures = msr("classif.ce")
#' param_set = ParamSet$new(params = list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   ParamInt$new("minsplit", lower = 1, upper = 10)))
#'
#' terminator = TerminatorEvals$new(5)
#' inst = TuningInstance$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   param_set = param_set,
#'   terminator = terminator
#' )
#' inst$eval_batch(data.table(cp = c(0.05, 0.01), minsplit = c(5, 3)))
#' inst$archive()
TuningInstance = R6Class("TuningInstance",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    param_set = NULL,
    terminator = NULL,
    bm_args = NULL,
    bmr = NULL,
    start_time = NULL,

    initialize = function(task, learner, resampling, measures, param_set, terminator, bm_args = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$param_set = assert_param_set(param_set, must_bounded = TRUE, no_untyped = TRUE)
      self$terminator = assert_terminator(terminator)
      self$bm_args = assert_list(bm_args, names = "unique")
      self$bmr = BenchmarkResult$new(data.table())
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
      catf(str_indent("* bm_args:", as_short_string(self$bm_args)))
      print(self$param_set)
      catf("Archive:")
      print(self$archive())
    },

    # evaluates all points in a design
    # possibly transforms the data before using the trafo from self$param set
    eval_batch = function(dt) {
      # dt can contain missings because of non-fullfilled deps
      assert_data_table(dt, any.missing = TRUE, min.rows = 1L, min.cols = 1L)
      # this checks the validity of dt lines in the paramset
      design = Design$new(self$param_set, dt, remove_dupl = FALSE)

      lg$info("Evaluating %i configurations", nrow(dt))
      lg$info("%s", capture.output(dt))

      # trafo and remove non-satisfied deps
      parlist = design$transpose(trafo = TRUE, filter_na = TRUE)

      # clone learners same length as parlist and set the configs
      lrns = lapply(parlist, function(xs) {
        lrn = self$learner$clone(deep = TRUE)
        lrn$param_set$values = insert_named(lrn$param_set$values, xs)
        return(lrn)
      })

      # eval via benchmark and check terminator
      d = benchmark_grid(tasks = list(self$task), learners = lrns, resamplings = list(self$resampling))
      bmr = invoke(benchmark, design = d, .args = self$bm_args)
      # store evaluated results
      self$bmr$combine(bmr)

      # if the terminator is positive throw condition of class "terminated_message" that we can tryCatch
      if (self$terminator$is_terminated(self)) {
        stop(messageCondition("TuningInstance terminated", class = "terminated_message"))
      }

      # get aggregated measures in dt, return them
      mids = map_chr(self$measures, "id")
      return(bmr$aggregate(measures = self$measures, ids = FALSE)[, mids, with = FALSE])
    },

    archive = function(unnest = TRUE) {
      dt = self$bmr$aggregate(measures = self$measures, params = TRUE)
      if (unnest) {
        dt = mlr3misc::unnest(dt, "params")
      }
      return(dt)
    },

    best = function(measure = NULL, ties_method = "random") {
      measure = if (is.null(measure)) self$measures[[1L]] else assert_measure(measure, task = self$task, learner = self$learner)

      # check that we are only using contained measures
      assert_choice(measure$id, map_chr(self$measures, "id"))
      tab = self$bmr$aggregate(measure, ids = FALSE)
      best = if (measure$minimize) which_min else which_max
      # this asserts the ties_methods
      tab$resample_result[[best(tab[[measure$id]], ties_method = ties_method)]]
    }

  ),

  active = list(
    n_evals = function() self$bmr$n_resample_results
  )
)
