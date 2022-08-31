#' @title Class for Multi Criteria Tuning
#'
#' @include TuningInstanceSingleCrit.R
#'
#' @description
#' The [TuningInstanceMultiCrit] specifies a tuning task for [Tuners][Tuner].
#' The instance is not created by the user but internally when the function [tune()] is called with more than one [mlr3::Measure].
#'
#' @inherit TuningInstanceSingleCrit details
#' @inheritSection TuningInstanceSingleCrit Resources
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_xdt
#' @template param_learner_param_vals
#'
#' @export
#' @examples
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' instance = tune(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measures = msrs(c("classif.ce", "classif.acc")),
#'   term_evals = 4)
#'
#' # get optimized hyperparameters
#' instance$result
TuningInstanceMultiCrit = R6Class("TuningInstanceMultiCrit",
  inherit = OptimInstanceMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' This defines the resampled performance of a learner on a task, a
    #' feasibility region for the parameters the tuner is supposed to optimize,
    #' and a termination criterion.
    initialize = function(task, learner, resampling, measures, terminator, search_space = NULL, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, evaluate_default = FALSE) {
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
      measures = assert_measures(as_measures(measures, clone = TRUE), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      archive = ArchiveTuning$new(search_space, codomain, check_values)
      objective = ObjectiveTuning$new(task, learner, resampling, measures, store_benchmark_result, store_models, check_values, allow_hotstart, keep_hotstart_stack, archive)

      super$initialize(objective, search_space, terminator)
      # super class of instance initializes default archive, overwrite with tuning archive
      self$archive = archive
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
        learner_param_vals = self$objective$learner$param_set$values
        if (length(learner_param_vals) == 0) learner_param_vals = list()
        learner_param_vals = replicate(nrow(ydt), list(learner_param_vals))
      }

      opt_x = transform_xdt_to_xss(xdt, self$search_space)
      if (length(opt_x) == 0) opt_x = replicate(length(ydt), list())
      learner_param_vals = Map(insert_named, learner_param_vals, opt_x)
      xdt = cbind(xdt, learner_param_vals)
      super$assign_result(xdt, ydt)
    }
  ),

  active = list(
    #' @field result_learner_param_vals (`list()`)\cr
    #'   List of param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals

    }
  ),

  private = list(
    .evaluate_default = NULL
  )
)
