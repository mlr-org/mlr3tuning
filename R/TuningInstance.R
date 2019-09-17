#' @title TuningInstance Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Specifies a general tuning scenario, including performance evaluator and archive for Tuners to
#' act upon. This class encodes the black box objective function,
#' that a [Tuner] has to optimize. It allows the basic operations of querying the objective
#' at design points (see `$eval_batch()`), storing the evaluations in an internal archive
#' and querying the archive (see `$archive()`).
#'
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' After a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#'
#' A list of multiple measures can be passed to the instance, and they will always be all evaluated.
#' But single-criteria tuners always optimize the first measure in the passed list.
#'
#' The tuner is also supposed to store its final result, consisting of a selected hyperparameter configuration,
#' and associated estimated performance values in the instance slots `result_config` and `result_perf`.
#'
#' @section Construction:
#' ```
#' inst = TuningInstance$new(task, learner, resampling, measures, param_set, terminator, bm_args = list())
#' ```
#' This defines the resampled performance of a learner on a task, a feasibility region
#' for the parameters the tuner is supposed to optimize, and a termination criterion.
#'
#' * `task` :: [mlr3::Task] | [mlr3::mlr_sugar].
#' * `learner` :: [mlr3::Learner] | [mlr3::mlr_sugar].
#' * `resampling` :: [mlr3::Resampling] | [mlr3::mlr_sugar].
#'   Note that the resampling is instantiated at the beginning so that all configurations
#'   are evaluated on the same data splits.
#' * `measures` :: list of [mlr3::Measure] | [mlr3::mlr_sugar].
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Hyperparameter search space.
#' * `terminator` :: [Terminator].
#' * `bm_args` :: named `list()`\cr
#'   Further arguments for [mlr3::benchmark()].
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
#'   This is set in the beginning of `$tune()` of [Tuner].
#' * `result_config` :: named `list`\cr
#'   Optimal configuration of settings, from the feasible `param_set`.
#'   The tuner writes the estimated optimal configuration of the learner here.
#'   Must be a list of settings only of parameters from `param_set`.
#'   The configuration must be a valid and pass the `check` / `assert` function of the [paradox::ParamSet].
#' * `result_config_complete` :: named `list`\cr
#'   Convenience access. The same as `result_config`, but if the learner had some extra parameters
#'   statically set before tuning, these are also included here.
#' * `result_perf` :: named `numeric()`\cr
#'   Vector of estimated performance values of optimal configuration.
#'   The tuner writes the estimated performance of `result_config` here.
#'   Must be a vector of performance measures, named with performance IDs,
#'   regarding all elements in `measures`.
#'
#' @section Methods:
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> named `list()`\cr
#'   Evaluates all hyperparameter configurations in `dt` through resampling, where each configuration is a row, and columns are scalar parameters.
#'   Updates the internal [BenchmarkResult] `$bmr` by reference, and returns a named list with two elements:
#'   * `"batch_nr"`: Number of the new batch.
#'     This number is calculated in an auto-increment fashion and stored also stored inside the [BenchmarkResult] as column `batch_nr`
#'   * `"uhashes"`: hashes of the added [ResampleResult]s.
#'   * `"perf"`: A data.table of evaluated performances for `dt`. Has the same nr of rows as `dt`, and the same nr of columns as `measures`. Columns are named with measure-IDs.
#'     A cell entry is the (aggregated) performance of that configuration for that measure.
#'
#'   After a batch-evaluation the [Terminator] is checked, if it is positive, an exception of class `terminated_error` is raised.
#'   This function should be internally called by the tuner.
#'
#' * `tuner_objective(x)`\cr
#'   `numeric()` -> `numeric(1)`\cr
#'   Evaluates a hyperparameter configuration of only numeric values, and returns a scalar objective value,
#'   where the return value is negated if the measure is maximized.
#'   Internally, `$eval_batch()` is called with a single row.
#'   This function serves as a objective function for tuners of numeric spaces - which should always be minimized.
#'
#' * `best(measure = NULL)`\cr
#'   ([mlr3::Measure] | [mlr3::mlr_sugar], `character(1)`) -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according `measure` (default is the first measure in `$measures`).
#'   In case of ties, one of the tied values is selected randomly.
#'
#' * `archive(unnest = TRUE)`
#'   `logical(1)` -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s `$aggregate()` method.
#'   If `unnest` is `TRUE` (default), hyperparameter configuration settings are stored in separate columns instead of inside a list column.
#'
#' @family TuningInstance
#' @export
#' @examples
#' library(data.table)
#' library(paradox)
#' library(mlr3)
#'
#' # Objects required to define the performance evaluator:
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measures = msr("classif.ce")
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   ParamInt$new("minsplit", lower = 1, upper = 10))
#' )
#'
#' terminator = term("evals", n_evals = 5)
#' inst = TuningInstance$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   param_set = param_set,
#'   terminator = terminator
#' )
#'
#' # first 4 points as cross product
#' design = CJ(cp = c(0.05, 0.01), minsplit = c(5, 3))
#' inst$eval_batch(design)
#' inst$archive()
#'
#' # try more points, catch the eventually raised terminated message
#' tryCatch(
#'   inst$eval_batch(data.table(cp = 0.01, minsplit = 7)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' # try another point although the budget is now exhausted
#' tryCatch(
#'   inst$eval_batch(data.table(cp = 0.01, minsplit = 9)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' inst$archive()
#'
#' ### Error handling
#' # get a learner which breaks with 50% probability
#' # set encapsulation + fallback
#' learner = lrn("classif.debug", error_train = 0.5)
#' learner$encapsulate = c(train = "evaluate", predict = "evaluate")
#' learner$fallback = lrn("classif.featureless")
#'
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("x", lower = 0, upper = 1)
#' ))
#'
#' inst = TuningInstance$new(
#'   task = tsk("wine"),
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   param_set = param_set,
#'   terminator = term("evals", n_evals = 5)
#' )
#'
#' tryCatch(
#'   inst$eval_batch(data.table(x = 1:5 / 5)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' archive = inst$archive()
#'
#' # column errors: multiple errors recorded
#' print(archive)
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
      self$bmr$rr_data[, ("batch_nr") := integer()]
      self$resampling$instantiate(self$task)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(self$format())
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
      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }

      # dt can contain missings because of non-fulfilled dependencies
      assert_data_table(dt, any.missing = TRUE, min.rows = 1L, min.cols = 1L)

      # this checks the validity of dt lines in the paramset
      design = Design$new(self$param_set, dt, remove_dupl = FALSE)

      lg$info("Evaluating %i configurations", nrow(dt))
      lg$info(capture.output(print(dt, class = FALSE, row.names = FALSE, print.keys = FALSE)))

      # trafo and remove non-satisfied deps
      parlist = design$transpose(trafo = TRUE, filter_na = TRUE)

      # clone learners same length as parlist and set the configs
      lrns = lapply(parlist, function(xs) {
        lrn = self$learner$clone(deep = TRUE)
        lrn$param_set$values = insert_named(lrn$param_set$values, xs)
        return(lrn)
      })

      # eval via benchmark and check terminator
      d = data.table(task = list(self$task), learner = lrns, resampling = list(self$resampling))
      bmr = invoke(benchmark, design = d, .args = self$bm_args)

      # add column "batch_nr"
      batch_nr = self$bmr$rr_data$batch_nr
      batch_nr = if (length(batch_nr)) max(batch_nr) + 1L else 1L
      bmr$rr_data[, ("batch_nr") := batch_nr]

      # store evaluated results
      self$bmr$combine(bmr)

      mids = ids(self$measures)
      if (lg$threshold >= 400) {
        # somewhat bad code, but
        # - i dont know how to reference the current level as "info" instead of "400" for the threshold in lgr
        # - i only want to aggregate when "info" is set and we want to remove the aggregation in eval_batch later
        a = bmr$aggregate(measures = self$measures, ids = FALSE)[, mids, with = FALSE]
        lg$info("Result:")
        lg$info(capture.output(print(cbind(dt, a), class = FALSE, row.names = FALSE, print.keys = FALSE)))
      }

      # if the terminator is positive throw condition of class "terminated_error" that we can tryCatch
      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }
      perf = bmr$aggregate(measures = self$measures, ids = FALSE)[, mids, with = FALSE]
      return(list(batch_nr = batch_nr, uhashes = bmr$uhashes, perf = perf))
    },

    tuner_objective = function(x) {
      assert_numeric(x, len = self$param_set$length)
      self$param_set$assert(as.list(x))
      m = self$measures[[1L]]
      d = setnames(setDT(as.list(x)), self$param_set$ids())
      z = self$eval_batch(d)
      y = z$perf[[m$id]]
      if (m$minimize) y else -y
    },

    archive = function(unnest = TRUE) {
      dt = self$bmr$aggregate(measures = self$measures, params = TRUE, conditions = TRUE)
      if (unnest) {
        dt = mlr3misc::unnest(dt, "params")
      }
      setcolorder(dt, c("nr", "batch_nr"))
      return(dt)
    },

    best = function(measure = NULL) {
      if (is.null(measure)) {
        measure = self$measures[[1L]]
      } else {
        measure = as_measure(measure, task_type = self$task$task_type)
        # check that we are only using contained measures
        assert_choice(measure$id, map_chr(self$measures, "id"))
      }
      assert_measure(measure, task = self$task, learner = self$learner)
      if (is.na(measure$minimize))
        stopf("Measure '%s' has minimize = NA and hence cannot be tuned", measure$id)

      tab = self$bmr$aggregate(measure, ids = FALSE)
      y = tab[[measure$id]]
      if (allMissing(y))
        stopf("No non-missing performance value stored")

      best = if (measure$minimize) which_min else which_max
      tab$resample_result[[best(y, na_rm = TRUE)]]
    }
  ),

  active = list(
    n_evals = function() self$bmr$n_resample_results,

    result_perf = function(rhs) {
      if (missing(rhs))
        return(private$.result_perf)
      # result_perf must be numeric and cover all measures
      assert_numeric(rhs)
      assert_names(names(rhs), permutation.of = ids(self$measures))
      private$.result_perf = rhs
    },

    result_config = function(rhs) {
      if (missing(rhs))
        return(private$.result_config)
      assert_list(rhs)
      self$param_set$assert(rhs)
      private$.result_config = rhs
    },

    result_config_complete = function() {
      rc = private$.result_config
      res = self$learner$param_set$values
      insert_named(res, rc)
    }
  ),

  private = list(
    .result_config = NULL,
    .result_perf = NULL
  )
)
