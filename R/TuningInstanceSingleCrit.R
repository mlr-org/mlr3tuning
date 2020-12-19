#' @title Single Criterion Tuning Instance
#'
#' @description
#' Specifies a general single-criteria tuning scenario, including objective
#' function and archive for Tuners to act upon. This class stores an
#' `ObjectiveTuning` object that encodes the black box objective function which
#' a [Tuner] has to optimize. It allows the basic operations of querying the
#' objective at design points (`$eval_batch()`), storing the evaluations in the
#' internal `Archive` and accessing the final result (`$result`).
#'
#' Evaluations of hyperparameter configurations are performed in batches by
#' calling [mlr3::benchmark()] internally. Before a batch is evaluated, the
#' [bbotk::Terminator] is queried for the remaining budget. If the available
#' budget is exhausted, an exception is raised, and no further evaluations can
#' be performed from this point on.
#'
#' The tuner is also supposed to store its final result, consisting of a
#' selected hyperparameter configuration and associated estimated performance
#' values, by calling the method `instance$assign_result`.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_search_space
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_xdt
#' @template param_learner_param_vals
#'
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
#' measure = msr("classif.ce")
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#'   ParamInt$new("minsplit", lower = 1, upper = 10))
#' )
#'
#' terminator = trm("evals", n_evals = 5)
#' inst = TuningInstanceSingleCrit$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measure = measure,
#'   search_space = param_set,
#'   terminator = terminator
#' )
#'
#' # first 4 points as cross product
#' design = CJ(cp = c(0.05, 0.01), minsplit = c(5, 3))
#' inst$eval_batch(design)
#' inst$archive
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
#' inst$archive
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
#' inst = TuningInstanceSingleCrit$new(
#'   task = tsk("wine"),
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   search_space = param_set,
#'   terminator = trm("evals", n_evals = 5)
#' )
#'
#' tryCatch(
#'   inst$eval_batch(data.table(x = 1:5 / 5)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' archive = as.data.table(inst$archive)
#'
#' # column errors: multiple errors recorded
#' print(archive)
TuningInstanceSingleCrit = R6Class("TuningInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a
    #' feasibility region for the parameters the tuner is supposed to optimize,
    #' and a termination criterion.
    initialize = function(task, learner, resampling, measure,
      terminator, search_space = NULL, store_benchmark_result = TRUE,
      store_models = FALSE, check_values = FALSE) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token")) > 0) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = learner$param_set$search_space()
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      }

      measure = as_measure(measure)
      obj = ObjectiveTuning$new(task = task, learner = learner,
        resampling = resampling, measures = list(measure),
        store_benchmark_result = store_benchmark_result,
        store_models = store_models, check_values = check_values)
      super$initialize(obj, search_space, terminator)
      self$archive = ArchiveTuning$new(search_space = search_space,
        codomain = self$objective$codomain, check_values = check_values)
      self$objective$archive = self$archive
    },

    #' @description
    #' The [Tuner] object writes the best found point
    #' and estimated performance value here. For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #'   Optimal outcome.
    assign_result = function(xdt, y, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      assert_list(learner_param_vals, null.ok = TRUE, names = "named")
      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$get_values(type = "without_token")
      }
      opt_x = transform_xdt_to_xss(xdt, self$search_space)[[1]]
      learner_param_vals = insert_named(learner_param_vals, opt_x)
      # ugly but necessary to maintain list column correctly
      if (length(learner_param_vals) == 1) {
        learner_param_vals = list(learner_param_vals)
      }
      set(xdt, j = "learner_param_vals", value = list(learner_param_vals))
      super$assign_result(xdt, y)
    }
  ),

  active = list(
    #' @field result_learner_param_vals (`list()`)\cr
    #' Param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals[[1]]
    }
  )
)
