#' @title Class for Single Criterion Tuning
#
#' @description
#' The [TuningInstanceSingleCrit] specifies a tuning problem for [Tuners][Tuner].
#' The function [ti()] creates a [TuningInstanceSingleCrit] and the function [tune()] creates an instance internally.
#'
#' @details
#' The instance contains an [ObjectiveTuning] object that encodes the black box objective function a [Tuner] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_batch()`).
#' This operation is usually done by the [Tuner].
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' The evaluated hyperparameter configurations are stored in an [Archive][ArchiveTuning] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The tuner is also supposed to store its final result, consisting of a  selected hyperparameter configuration and associated estimated performance values, by calling the method `instance$assign_result`.
#'
#' @inheritSection ArchiveTuning Analysis
#'
#' @section Resources:
#' * [book chapter](https://mlr3book.mlr-org.com/optimization.html#tuning) on hyperparameter optimization.
#' * [book chapter](https://mlr3book.mlr-org.com/optimization.html#searchspace) on tuning spaces.
#' * [gallery post](https://mlr-org.com/gallery/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/practical-tuning-series-tune-a-support-vector-machine.html) on tuning.
#' * [mlr3tuningspaces](https://mlr3tuningspaces.mlr-org.com/) extension package.
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
#' @template param_evaluate_default
#' @template param_callbacks
#' @template param_xdt
#' @template param_learner_param_vals
#'
#' @export
#' @examples
#' # get learner and define search space
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' # construct tuning instance
#' instance = ti(
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # get tuner
#' tuner = tnr("random_search", batch_size = 2)
#'
#' # tune classification tree on pima data set
#' tuner$optimize(instance)
#'
#' # get result
#' instance$result
TuningInstanceSingleCrit = R6Class("TuningInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measure = NULL, terminator, search_space = NULL, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, evaluate_default = FALSE, callbacks = list()) {
      private$.evaluate_default = assert_flag(evaluate_default)
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
      measures = assert_measures(as_measures(measure, task_type = task$task_type, clone = TRUE), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      archive = ArchiveTuning$new(search_space, codomain, check_values)
      objective = ObjectiveTuning$new(task, learner, resampling, measures, store_benchmark_result, store_models, check_values, allow_hotstart, keep_hotstart_stack, archive, callbacks)

      super$initialize(objective, search_space, terminator, callbacks = callbacks)
      # super class of instance initializes default archive, overwrite with tuning archive
      self$archive = archive
    },

    #' @description
    #' The [Tuner] object writes the best found point and estimated performance value here. For internal use.
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
  ),

  private = list(
    .evaluate_default = NULL
  )
)
