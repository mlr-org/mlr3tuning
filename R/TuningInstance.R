#' @title TuningInstance
#'
#' @description
#' Specifies a general tuning scenario, including objective function
#' and archive for Tuners to act upon. This class stores an `ObjectiveTuning`
#' object that encodes the black box objective function which a [Tuner] has to
#' optimize. It allows the basic operations of querying the objective
#' at design points (`$eval_batch()`), storing the evaluations in the internal
#' `Archive` and accessing the final result (`$result`).
#'
#' Evaluations of hyperparameter configurations are performed in batches by
#' calling [mlr3::benchmark()] internally. Before a batch is evaluated, the
#' [Terminator] is queried for the remaining budget. If the available budget is
#' exhausted, an exception is raised, and no further evaluations can be
#' performed from this point on.
#'
#' The tuner is also supposed to store its final result, consisting of a
#' selected hyperparameter configuration and associated estimated performance
#' values, by calling the method `instance$assign_result`.
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
#' terminator = term("evals", n_evals = 5)
#' inst = TuningInstance$new(
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
#' inst = TuningInstance$new(
#'   task = tsk("wine"),
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   search_space = param_set,
#'   terminator = term("evals", n_evals = 5)
#' )
#'
#' tryCatch(
#'   inst$eval_batch(data.table(x = 1:5 / 5)),
#'   terminated_error = function(e) message(as.character(e))
#' )
#'
#' archive = inst$archive$data()
#'
#' # column errors: multiple errors recorded
#' print(archive)
TuningInstance = R6Class("TuningInstance",
  inherit = OptimInstance,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a
    #' feasibility region for the parameters the tuner is supposed to optimize,
    #' and a termination criterion.
    #'
    #' @param task ([mlr3::Task])
    #'
    #' @param learner ([mlr3::Learner])
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Note that uninstantiated resamplings are instantiated during construction
    #' so that all configurations are evaluated on the same data splits.
    #'
    #' @param measure ([mlr3::Measure])\cr
    #' Measure to optimize.
    #'
    #' @param search_space ([paradox::ParamSet])
    #'
    #' @param terminator ([Terminator])
    #' @param store_models `logical(1)`
    initialize = function(task, learner, resampling, measure, search_space,
      terminator, store_models = FALSE) {
        measure = as_measure(measure)
        obj = ObjectiveTuning$new(task = task, learner = learner,
          resampling = resampling, measures = list(measure),
          store_models = store_models)
        super$initialize(obj, search_space, terminator)
    },

    #' @description
    #' The [Tuner] object writes the best found point
    #' and estimated performance value here. For internal use.
    #'
    #' @param xdt (`data.table`)\cr
    #'   x values as `data.table` with one row.
    #'   Contains the value in the *search space* of the [TuningInstance] object.
    #'   Can contain additional columns for extra information.
    #' @param y (`numeric(1)`)\cr
    #'   Optimal outcome.
    #' @param learner_param_vals (`list()`)\cr
    #'   Fixed parameter values of the learner that are neither part of the *search space* nor the domain.
    #    Named list.
    assign_result = function(xdt, y, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      assert_list(learner_param_vals, null.ok = TRUE, names = "named")
      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$values
      }
      opt_x = transform_xdt_to_xss(xdt, self$search_space)[[1]]
      learner_param_vals = insert_named(learner_param_vals, opt_x)
      # ugly but necessary to maintain list column correctly
      if (length(learner_param_vals) == 1) {
        learner_param_vals = list(learner_param_vals)
      }
      xdt[, learner_param_vals := list(learner_param_vals)]
      super$assign_result(xdt, y)
    }
  ),

  active = list(
    #' @field result_learner_param_vals (`list()`)
    #'   Param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals[[1]]
    }
  )
)
