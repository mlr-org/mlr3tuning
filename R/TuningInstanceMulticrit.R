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
    #'
    #' @param task ([mlr3::Task]).
    #'
    #' @param learner ([mlr3::Learner]).
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Note that uninstantiated resamplings are instantiated during construction
    #' so that all configurations are evaluated on the same data splits.
    #'
    #' @param measures (list of [mlr3::Measure])\cr
    #' Measures to optimize.
    #'
    #' @param search_space ([paradox::ParamSet]).
    #'
    #' @param terminator ([bbotk::Terminator]).
    #' @param store_models (`logical(1)`).
    #'
    #' @param check_values (`logical(1)`).
    #' Check the parameters before the evaluation and the results for
    #' validity?
    initialize = function(task, learner, resampling, measures, search_space,
      terminator, store_models = FALSE, check_values = TRUE) {
        obj = ObjectiveTuning$new(task = task, learner = learner,
          resampling = resampling, measures = measures,
          store_models = store_models, check_values = check_values)
        super$initialize(obj, search_space, terminator)
    },

    #' @description
    #' The [Tuner] object writes the best found points
    #' and estimated performance values here. For internal use.
    #'
    #' @param xdt (`data.table::data.table()`)\cr
    #' x values as `data.table`. Each row is one point. Contains the value in
    #' the *search space* of the [TuningInstanceMultiCrit] object. Can contain
    #' additional columns for extra information.
    #' @param ydt (`data.table::data.table()`)\cr
    #' Optimal outcomes, e.g. the Pareto front.
    #' @param learner_param_vals (`list()`)\cr
    #' Fixed parameter values of the learner that are neither part of the
    #` *search space* nor the domain. List of named lists.
    assign_result = function(xdt, ydt, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly

      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$values
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
