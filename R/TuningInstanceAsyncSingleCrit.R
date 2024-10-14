#' @title Single Criterion Tuning with Rush
#
#' @description
#' The `TuningInstanceAsyncSingleCrit` specifies a tuning problem for a [TunerAsync].
#' The function [ti_async()] creates a [TuningInstanceAsyncSingleCrit] and the function [tune()] creates an instance internally.
#'
#' @details
#' The instance contains an [ObjectiveTuningAsync] object that encodes the black box objective function a [Tuner] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_async()`).
#' This operation is usually done by the [Tuner].
#' Hyperparameter configurations are asynchronously sent to workers and evaluated by calling [mlr3::resample()].
#' The evaluated hyperparameter configurations are stored in the [ArchiveAsyncTuning] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The tuner is also supposed to store its final result, consisting of a selected hyperparameter configuration and associated estimated performance values, by calling the method `instance$.assign_result`.
#'
#' @inheritSection TuningInstanceBatchSingleCrit Default Measures
#' @inheritSection ArchiveAsyncTuning Analysis
#' @inheritSection TuningInstanceBatchSingleCrit Resources
#' @inheritSection TuningInstanceBatchSingleCrit Extension Packages
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
#' @template param_callbacks
#' @template param_rush
#'
#' @template param_internal_search_space
#' @template param_xdt
#' @template param_learner_param_vals
#' @template param_internal_tuned_values
#'
#' @template field_internal_search_space
#'
#' @export
TuningInstanceAsyncSingleCrit = R6Class("TuningInstanceAsyncSingleCrit",
  inherit = OptimInstanceAsyncSingleCrit,
  public = list(

    internal_search_space = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measure = NULL,
      terminator,
      search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL
      ) {
      require_namespaces("rush")
      learner = assert_learner(as_learner(learner, clone = TRUE))

      # tune token and search space
      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token"))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = as_search_space(learner)
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      } else {
        search_space = as_search_space(search_space)
      }

      # internal search space
      internal_tune_ids = keep(names(search_space$tags), map_lgl(search_space$tags, function(tag) "internal_tuning" %in% tag))
      if (length(internal_tune_ids)) {
        self$internal_search_space = search_space$subset(internal_tune_ids)

        if (self$internal_search_space$has_trafo) {
          stopf("Inner tuning and parameter transformations are currently not supported.")
        }

        search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))

        # the learner dictates how to interpret the to_tune(..., inner)
        learner$param_set$set_values(.values = learner$param_set$convert_internal_search_space(self$internal_search_space))
      }

      if (is.null(rush)) rush = rush::rsh()

      # create codomain from measure
      measures = assert_measures(as_measures(measure, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      archive = ArchiveAsyncTuning$new(
        search_space = search_space,
        codomain = codomain,
        rush = rush,
        internal_search_space = self$internal_search_space
      )

      objective = ObjectiveTuningAsync$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks,
        internal_search_space = self$internal_search_space)

      super$initialize(
        objective,
        search_space,
        terminator,
        callbacks = callbacks,
        rush = rush,
        archive = archive)
    },

    #' @description
    #' The [TunerAsync] object writes the best found point and estimated performance value here.
    #' For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    #' @param xydt (`data.table::data.table()`)\cr
    #'   Point, outcome, and additional information.
    assign_result = function(xdt, y, learner_param_vals = NULL, xydt = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      assert_list(learner_param_vals, null.ok = TRUE, names = "named")

      # extract internal tuned values
      if ("internal_tuned_values" %in% names(xydt)) {
        set(xdt, j = "internal_tuned_values", value = list(xydt[["internal_tuned_values"]]))
      }

      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$values
      }
      opt_x = unlist(transform_xdt_to_xss(xdt, self$search_space), recursive = FALSE)
      learner_param_vals = insert_named(learner_param_vals, opt_x)

      # maintain list column
      if (length(learner_param_vals) < 2 | !nrow(xdt)) learner_param_vals = list(learner_param_vals)

      # disable internal tuning
      if (!is.null(xdt$internal_tuned_values)) {
        learner = self$objective$learner$clone(deep = TRUE)
        learner_param_vals = insert_named(learner_param_vals, xdt$internal_tuned_values[[1]])
        learner$param_set$set_values(.values = learner_param_vals)
        learner$param_set$disable_internal_tuning(self$internal_search_space$ids())
        learner_param_vals = learner$param_set$values
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
  ),

  private = list(

    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextAsyncTuning$new(self, optimizer)
      self$objective$context = context
    }
  )
)
