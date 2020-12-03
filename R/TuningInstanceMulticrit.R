#' @title Multi Criteria Tuning Instance
#'
#' @description
#' Specifies a general multi-criteria tuning scenario, including objective
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
#' @template param_measures
#' @template param_search_space
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_xdt
#' @template param_learner_param_vals
#'
#' @export
TuningInstanceMultiCrit = R6Class("TuningInstanceMultiCrit",
  inherit = OptimInstanceMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a
    #' feasibility region for the parameters the tuner is supposed to optimize,
    #' and a termination criterion.
    initialize = function(task, learner, resampling, measures,
      terminator, search_space = NULL, store_models = FALSE,
      check_values = FALSE, store_benchmark_result = TRUE) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token")) > 0) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = learner$param_set$search_space()
      }

      obj = ObjectiveTuning$new(
        task = task, learner = learner,
        resampling = resampling, measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models, check_values = check_values)
      super$initialize(obj, search_space, terminator)
      self$archive = ArchiveTuning$new(search_space = search_space,
        codomain = self$objective$codomain, check_values = check_values)
      self$objective$archive = self$archive
    },

    #' @description
    #' The [Tuner] object writes the best found points
    #' and estimated performance values here. For internal use.
    #'
    #' @param ydt (`data.table::data.table()`)\cr
    #'   Optimal outcomes, e.g. the Pareto front.
    assign_result = function(xdt, ydt, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$get_values(type = "without_token")
        learner_param_vals = replicate(nrow(xdt), learner_param_vals, simplify = FALSE)
      }
      assert_list(learner_param_vals, len = nrow(xdt))
      opt_x = transform_xdt_to_xss(xdt, self$search_space)
      xdt$learner_param_vals = Map(insert_named, learner_param_vals, opt_x)
      super$assign_result(xdt, ydt)
    }
  ),

  active = list(
    #' @field result_learner_param_vals (`list()`)\cr
    #'   List of param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals

    }
  )
)
