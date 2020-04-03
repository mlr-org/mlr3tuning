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
   inherit = OptimInstance,
   public = list(

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
    #' @param store_models `logical(1)`.
    initialize = function(task, learner, resampling, measures, param_set, terminator, store_models = FALSE) {
      obj = ObjectiveFSelect$new(task = task, learner = learner,
        resampling = resampling, measures = measures, param_set = param_set,
        store_models = store_models)
      super$initialize(obj, param_set, terminator)
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
      assert_names(names(perf), permutation.of = ids(self$objective$measures))
      private$.result = list(tune_x = tune_x, perf = perf)
    }
  ),

  active = list(
    #' @field result (named `list()`)\cr
    #'   Result of the tuning, i.e., the optimal configuration and its estimated performance:
    result = function() {
      tune_x = private$.result$tune_x
      perf = private$.result$perf
      # if ps has no trafo, just use the normal config
      trafo = if (is.null(self$objective$domain$trafo)) identity else self$param_set$trafo
      params = trafo(tune_x)
      params = insert_named(self$objective$learner$param_set$values, params)
      list(tune_x = tune_x, params = params, perf = perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
