#' @title TuningInstance Class
#'
#' @description
#' Specifies a general tuning scenario, including performance evaluator and archive for Tuners to
#' act upon. This class encodes the black box objective function,
#' that a [Tuner] has to optimize. It allows the basic operations of querying the objective
#' at design points (`$eval_batch()`), storing the evaluations in an internal archive
#' and querying the archive (`$archive()`).
#'
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' Before a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#'
#' A list of measures can be passed to the instance, and they will always be all evaluated.
#' However, single-criteria tuners optimize only the first measure.
#'
#' The tuner is also supposed to store its final result, consisting of a selected hyperparameter configuration
#' and associated estimated performance values, by calling the method `instance$assign_result`.
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
#' # try more points, catch the raised terminated message
#' tryCatch(
#'   inst$eval_batch(data.table(cp = 0.01, minsplit = 7)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' # try another point although the budget is now exhausted
#' # -> no extra evaluations
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

    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

    #' @field resampling ([mlr3::Resampling])\cr
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure]).
    measures = NULL,

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @field terminator ([Terminator]).
    terminator = NULL,

    #' @field bm_args (named `list()`)\cr
    #'   Further arguments for [mlr3::benchmark()].
    bm_args = NULL,

    #' @field bmr ([mlr3::BenchmarkResult])\cr
    #'   A benchmark result, container object for all performed [mlr3::ResampleResult]s
    #'   when evaluating hyperparameter configurations.
    bmr = NULL,

    #' @field start_time (`POSIXct(1)`)\cr
    #'   Time the tuning was started.
    #'   This is set in the beginning of `$tune()` of [Tuner].
    start_time = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a feasibility region
    #' for the parameters the tuner is supposed to optimize, and a termination criterion.
    #'
    #' @param task ([mlr3::Task]).
    #'
    #' @param learner ([mlr3::Learner]).
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #'   Note that uninstantiated resamplings are instantiated during construction so that all configurations
    #'   are evaluated on the same data splits.
    #'
    #' @param measures (list of [mlr3::Measure]).
    #'
    #' @param param_set ([paradox::ParamSet]).
    #'
    #' @param terminator ([Terminator]).
    #'
    #' @param bm_args (named `list()`)\cr
    #'   Further arguments for [mlr3::benchmark()].
    initialize = function(task, learner, resampling, measures, param_set, terminator, bm_args = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$param_set = assert_param_set(param_set, must_bounded = TRUE, no_untyped = TRUE)
      self$terminator = assert_terminator(terminator)
      self$bm_args = assert_list(bm_args, names = "unique")
      self$bmr = BenchmarkResult$new(data.table())
      self$bmr$rr_data[, c("batch_nr", "tune_x") := list(integer(), list())]
      if (!resampling$is_instantiated)
        self$resampling$instantiate(self$task)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf(self$format())
      catf(str_indent("* State: ", if(is.null(self$result$perf)) "Not tuned" else "Tuned"))
      catf(str_indent("* Task:", format(self$task)))
      catf(str_indent("* Learner:", format(self$learner)))
      catf(str_indent("* Measures:", map_chr(self$measures, "id")))
      catf(str_indent("* Resampling:", format(self$resampling)))
      catf(str_indent("* Terminator:", format(self$terminator)))
      catf(str_indent("* bm_args:", as_short_string(self$bm_args)))
      catf(str_indent("* n_evals:", self$n_evals))
      if(!is.null(self$result$perf)) {
        catf(str_indent("* Result perf:", as_short_string(as.list(self$result$perf))) )
        catf(str_indent("* Result tune_x:", as_short_string(self$result$tune_x)))
      }
      print(self$param_set)
    },

    #' @description
    #' Evaluates all hyperparameter configurations in `dt` through resampling and updates the internal [BenchmarkResult] `$bmr` by reference.
    #'
    #' Before each batch-evaluation, the [Terminator] is checked, and if it is positive, an exception of class `terminated_error` is raised.
    #' This function is intended to be internally called by a [Tuner].
    #'
    #' @param dt ([data.table::data.table()])\cr
    #'  Table of hyperparameter configurations where each configuration is a row, and columns are scalar parameters.
    #'
    #' @return Named `list()` with the following elements:
    #' * `"batch_nr"` (`integer(1)`):\cr
    #'   Number of the new batch.
    #'   This number is calculated in an auto-increment fashion and also stored inside the [BenchmarkResult] as column `batch_nr`.
    #' * `"uhashes"` (`character()`):\cr
    #'   Unique hashes of the added [ResampleResult]s.
    #' * `"perf"` ([data.table::data.table()]):\cr
    #'   Table of evaluated performances for each row of `dt`.
    #'   Has the same number of rows as `dt`, and the same number of columns as length of `measures`.
    #'   Columns are named with measure-IDs.
    #'   A cell entry is the (aggregated) performance of that configuration for that measure.
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

      # convert configs to lists and remove non-satisfied deps
      parlist_trafoed = design$transpose(trafo = TRUE, filter_na = TRUE)
      parlist_untrafoed = design$transpose(trafo = FALSE, filter_na = TRUE)

      # clone learners same length as parlist and set the configs (trafoed)
      lrns = lapply(parlist_trafoed, function(xs) {
        lrn = self$learner$clone(deep = TRUE)
        lrn$param_set$values = insert_named(lrn$param_set$values, xs)
        return(lrn)
      })

      # eval via benchmark and check terminator
      d = data.table(task = list(self$task), learner = lrns, resampling = list(self$resampling))
      bmr = invoke(benchmark, design = d, .args = self$bm_args)

      # rr_data: add column "batch_nr"
      batch_nr = self$bmr$rr_data$batch_nr
      batch_nr = if (length(batch_nr)) max(batch_nr) + 1L else 1L
      bmr$rr_data[, ("batch_nr") := batch_nr]

      # rr_data: add column "tune_x"
      bmr$rr_data[, ("tune_x") := list(parlist_untrafoed)]

      # store evaluated results
      self$bmr$combine(bmr)

      mids = ids(self$measures)
      if (lg$threshold >= 400) {
        # somewhat bad code, but
        # - i dont know how to reference the current level as "info" instead of "400" for the threshold in lgr
        # - i only want to aggregate when "info" is set and we want to remove the aggregation in eval_batch later
        a = bmr$aggregate(measures = self$measures, ids = FALSE)[, mids, with = FALSE]
        lg$info("Result of batch %i:", batch_nr)
        lg$info(capture.output(print(cbind(dt, a), class = FALSE, row.names = FALSE, print.keys = FALSE)))
        lg$info("%i configurations evaluated", self$n_evals)
      }

      perf = bmr$aggregate(measures = self$measures, ids = FALSE)[, mids, with = FALSE]
      return(list(batch_nr = batch_nr, uhashes = bmr$uhashes, perf = perf))
    },

    #' @description
    #' Evaluates a (untransformed) hyperparameter configuration of only numeric values, and returns a scalar objective value,
    #' where the return value is negated if the measure is maximized.
    #' Internally, `$eval_batch()` is called with a single row.
    #' This function serves as a objective function for tuners of numeric spaces - which should always be minimized.
    #'
    #' @param x (`numeric()`)\cr
    #'   Untransformed hyperparameter configuration.
    #'
    #' @return Objective value as `numeric(1)`.
    tuner_objective = function(x) {
      assert_numeric(x, len = self$param_set$length)
      self$param_set$assert(as.list(x))
      m = self$measures[[1L]]
      d = setnames(setDT(as.list(x)), self$param_set$ids())
      z = self$eval_batch(d)
      y = z$perf[[m$id]]
      if (m$minimize) y else -y
    },

    #' @description
    #'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s `$aggregate()` method.
    #'
    #'   Some important columns of this table are:
    #'   * All evaluated measures are included as numeric columns, named with their measure ID.
    #'   * `tune_x`: A list column that contains the parameter settings the tuner evaluated, without potential `trafo` applied.
    #'   * `params`: A list column that contains the parameter settings that were actually used in the learner.
    #'      Similar to column `tune_x`, but with potential `trafo` applied.
    #'      Also, if the learner had some extra parameters statically set before tuning, these are included here.
    #'   * `tune_x`: A named list of settings of feasible and untransformed parameters from `param_set`.
    #'
    #' @param unnest (`character(1)`)\cr
    #'   Can have the values `"no"`, `"tune_x"` or `"params"`.
    #'   If it is not set to `"no"`, settings of the respective list-column are stored in
    #'   separate columns instead of the list-column, and dependent, inactive parameters are encoded with `NA`.
    #'
    #' @return [data.table::data.table()].
    archive = function(unnest = "no") {
      assert_choice(unnest, c("no", "params", "tune_x"))
      dt = self$bmr$aggregate(measures = self$measures, params = TRUE, conditions = TRUE)
      # TODO: column reordering should become a mlr3misc function
      setcolorder(dt, c("nr", "batch_nr"))
      setcolorder(dt, c(head(names(dt), which(names(dt) == "params")), "tune_x"))
      if (unnest != "no") {
        dt = mlr3misc::unnest(dt, unnest)
      }
      dt[]
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according to [mlr3::Measure] `measure` (default is the first measure in `$measures`).
    #' In case of ties, one of the tied values is selected randomly.
    #'
    #' @param measure [mlr3::Measure].
    #'
    #' @return [mlr3::ResampleResult].
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

      which_best = if (measure$minimize) which_min else which_max
      best_index = which_best(y, na_rm = TRUE)
      tab$resample_result[[best_index]]
    },

    #' @description
    #' The tuner writes the best found list of settings and estimated performance values here. For internal use.
    #'
    #' @param tune_x (named `list()`)\cr
    #'   Hyperparameter configuration.
    #'
    #' @param perf (`numeric()`)\cr
    #'   Performance score for `tune_x`.
    #'
    #' @return Nothing.
    assign_result = function(tune_x, perf) {
      # result tune_x must be feasible for paramset
      self$param_set$assert(tune_x)
      # result perf must be numeric and cover all measures
      assert_numeric(perf)
      assert_names(names(perf), permutation.of = ids(self$measures))
      private$.result = list(tune_x = tune_x, perf = perf)
    }
  ),

  active = list(
    #' @field n_evals (`integer(1)`)\cr
    #'   Number of configuration evaluations stored in the container.
    n_evals = function() self$bmr$n_resample_results,

    #' @field result (named `list()`)\cr
    #'   Result of the tuning, i.e., the optimal configuration and its estimated performance:
    result = function() {
      tune_x = private$.result$tune_x
      perf = private$.result$perf
      # if ps has no trafo, just use the normal config
      trafo = if (is.null(self$param_set$trafo)) identity else self$param_set$trafo
      params = trafo(tune_x)
      params = insert_named(self$learner$param_set$values, params)
      list(tune_x = tune_x, params = params, perf = perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
