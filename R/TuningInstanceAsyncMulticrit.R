#' @title Multi-Criteria Tuning with Rush
#'
#' @include TuningInstanceBatchSingleCrit.R ArchiveAsyncTuning.R
#'
#' @description
#' The [TuningInstanceAsyncMultiCrit] specifies a tuning problem for a [Tuner].
#' The function [ti_async()] creates a [TuningInstanceAsyncMultiCrit] and the function [tune()] creates an instance internally.
#'
#' @inherit TuningInstanceAsyncSingleCrit details
#' @inheritSection TuningInstanceBatchMultiCrit Resources
#' @inheritSection ArchiveAsyncTuning Analysis
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
#' @template param_callbacks
#' @template param_rush
#'
#' @template param_xdt
#' @template param_learner_param_vals
#' @template param_internal_tuned_values
#' @template param_extra
#'
#' @template field_internal_search_space
#'
#' @export
TuningInstanceAsyncMultiCrit = R6Class("TuningInstanceAsyncMultiCrit",
  inherit = OptimInstanceAsyncMultiCrit,
  public = list(
    internal_search_space = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
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
      callbacks = assert_async_tuning_callbacks(as_callbacks(callbacks))

      # tune token and search space
      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token", check_required = FALSE))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }

      search_space_from_tokens = is.null(search_space)

      # convert tune token to search space
      search_space = if (is.null(search_space)) {
        learner$param_set$search_space()
      } else {
        as_search_space(search_space)
      }

      search_spaces = split_internal_search_space(search_space)
      search_space = search_spaces$search_space
      self$internal_search_space = search_spaces$internal_search_space

      # set internal search space
      if (!is.null(self$internal_search_space)) {
        # the learner dictates how to interpret the to_tune(..., inner)
        learner$param_set$set_values(.values = learner$param_set$convert_internal_search_space(self$internal_search_space))
      }

      # set learner parameter values
      if (search_space_from_tokens) {
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      }

      if (is.null(rush)) rush = rush::rsh()

      # create codomain from measure
      measures = assert_measures(as_measures(measures, task_type = task$task_type), task = task, learner = learner)
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
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        callbacks = callbacks,
        archive = archive,
        rush = rush)
    },

    #' @description
    #' The [TunerAsync] writes the best found points and estimated performance values here (probably the Pareto set / front).
    #' For internal use.
    #'
    #' @param ydt (`numeric(1)`)\cr
    #' Optimal outcomes, e.g. the Pareto front.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, ydt, learner_param_vals = NULL, extra = NULL, ...) {
      # assign for callbacks
      private$.result_xdt = xdt
      private$.result_ydt = ydt
      private$.result_learner_param_vals = learner_param_vals
      private$.result_extra = extra

      call_back("on_tuning_result_begin", self$objective$callbacks, self$objective$context)

      # extract internal tuned values
      if ("internal_tuned_values" %in% names(private$.result_extra)) {
        set(private$.result_xdt, j = "internal_tuned_values", value = list(private$.result_extra[["internal_tuned_values"]]))
      }

      # set the column with the learner param_vals that were not optimized over but set implicitly
      if (is.null(private$.result_learner_param_vals)) {
        private$.result_learner_param_vals = self$objective$learner$param_set$values
        if (length(private$.result_learner_param_vals) == 0) private$.result_learner_param_vals = list()
        private$.result_learner_param_vals = replicate(nrow(private$.result_ydt), list(private$.result_learner_param_vals))
      }

      opt_x = transform_xdt_to_xss(private$.result_xdt, self$search_space)
      if (length(opt_x) == 0) opt_x = replicate(length(private$.result_ydt), list())
      private$.result_learner_param_vals = Map(insert_named, private$.result_learner_param_vals, opt_x)

      # disable internal tuning
      if (!is.null(private$.result_xdt$internal_tuned_values)) {
        learner = self$objective$learner$clone(deep = TRUE)
        private$.result_learner_param_vals = pmap(list(private$.result_learner_param_vals, private$.result_xdt$internal_tuned_values), function(lpv, itv) {
          values = insert_named(lpv, itv)
          learner$param_set$set_values(.values = values, .insert = FALSE)
          learner$param_set$disable_internal_tuning(self$internal_search_space$ids())
          learner$param_set$values
        })
      }

      set(private$.result_xdt, j = "learner_param_vals", value = list(private$.result_learner_param_vals))

      super$assign_result(private$.result_xdt, private$.result_ydt)
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
    # intermediate objects
    .result_learner_param_vals = NULL,

    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextAsyncTuning$new(self, optimizer)
      self$objective$context = context
    }
  )
)
