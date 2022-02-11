#' @title Single Criterion Tuning Instance
#'
#' @description
#' Specifies a general single-criteria tuning scenario, including objective
#' function and archive for Tuners to act upon. This class stores an
#' [ObjectiveTuning] object that encodes the black box objective function which
#' a [Tuner] has to optimize. It allows the basic operations of querying the
#' objective at design points (`$eval_batch()`), storing the evaluations in the
#' internal [ArchiveTuning] and accessing the final result (`$result`).
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
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_xdt
#' @template param_learner_param_vals
#'
#' @export
#' @examples
#' library(data.table)
#'
#' # define search space
#' search_space = ps(
#'   cp = p_dbl(lower = 0.001, upper = 0.1),
#'   minsplit = p_int(lower = 1, upper = 10)
#' )
#'
#' # initialize instance
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = trm("evals", n_evals = 5)
#' )
#'
#' # generate design
#' design = data.table(cp = c(0.05, 0.01), minsplit = c(5, 3))
#'
#' # eval design
#' instance$eval_batch(design)
#'
#' # show archive
#' instance$archive
#'
#' ### error handling
#'
#' # get a learner which breaks with 50% probability
#' # set encapsulation + fallback
#' learner = lrn("classif.debug", error_train = 0.5)
#' learner$encapsulate = c(train = "evaluate", predict = "evaluate")
#' learner$fallback = lrn("classif.featureless")
#'
#' # define search space
#' search_space = ps(
#'  x = p_dbl(lower = 0, upper = 1)
#' )
#'
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("wine"),
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = trm("evals", n_evals = 5)
#' )
#'
#' instance$eval_batch(data.table(x = 1:5 / 5))
TuningInstanceSingleCrit = R6Class("TuningInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a
    #' feasibility region for the parameters the tuner is supposed to optimize,
    #' and a termination criterion.
    initialize = function(task, learner, resampling, measure = NULL, terminator, search_space = NULL,
      store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE,
      keep_hotstart_stack = FALSE) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token"))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = as_search_space(learner)
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      } else {
        search_space = as_search_space(search_space)
      }

      # create codomain from measure
      measures = assert_measures(as_measures(measure, task_type = task$task_type, clone = TRUE), task = task,
        learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      archive = ArchiveTuning$new(search_space, codomain, check_values)
      objective = ObjectiveTuning$new(task, learner, resampling, measures, store_benchmark_result, store_models,
        check_values, allow_hotstart, keep_hotstart_stack, archive)

      super$initialize(objective, search_space, terminator)
      # super class of instance initializes default archive, overwrite with tuning archive
      self$archive = archive
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
        learner_param_vals = self$objective$learner$param_set$values
      }
      opt_x = unlist(transform_xdt_to_xss(xdt, self$search_space), recursive = FALSE)
      learner_param_vals = insert_named(learner_param_vals, opt_x)

      # maintain list column
      if (length(learner_param_vals) < 2 | !nrow(xdt)) learner_param_vals = list(learner_param_vals)

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
