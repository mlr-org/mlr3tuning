#' @title TuningInstanceMulticrit
#'
#' @description
#' Specifies a general multi-criteria tuning scenario.
#' Inherits from [OptimInstanceMulticrit]
#'
#' @export
TuningInstanceMulticrit = R6Class("TuningInstanceMulticrit",
  inherit = OptimInstanceMulticrit,
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
    #' @param measures (list of [mlr3::Measure])\cr
    #' Measures to optimize.
    #'
    #' @param search_space ([paradox::ParamSet])
    #'
    #' @param terminator ([Terminator])
    #' @param store_models `logical(1)`
    initialize = function(task, learner, resampling, measures, search_space,
      terminator, store_models = FALSE) {
        obj = ObjectiveTuning$new(task = task, learner = learner,
          resampling = resampling, measures = measures,
          store_models = store_models)
        super$initialize(obj, search_space, terminator)
    },

    #' @description
    #' The [Tuner] object writes the best found points
    #' and estimated performance values here. For internal use.
    #'
    #' @param xdt (`data.table`)\cr
    #'   x values as `data.table`.
    #'   Each row is one point.
    #'   Contains the value in the *search space* of the [TuningInstance] object.
    #'   Can contain additional columns for extra information.
    #' @param ydt (`numeric(1)`)\cr
    #'   Optimal outcomes, e.g. the Pareto front.
    #' @param learner_param_vals (`list()`)\cr
    #'   Fixed parameter values of the learner that are neither part of the *search space* nor the domain.
    #    List of named lists.
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
    #' @field result_learner_param_vals (`list()`)
    #'   List of param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals

    }
  )
)
