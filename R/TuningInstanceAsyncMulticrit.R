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
#'
#' @export
TuningInstanceAsyncMultiCrit = R6Class("TuningInstanceAsyncMultiCrit",
  inherit = OptimInstanceAsyncMultiCrit,
  public = list(

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

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token"))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = as_search_space(learner)
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      } else {
        search_space = as_search_space(search_space)
      }

      # modifies tuning instance in-place and adds the internal tuning callback
      res = init_internal_search_space(self, private, super, search_space, store_benchmark_result, learner,
        callbacks, batch = FALSE)

      private$.internal_search_space = res$internal_search_space
      callbacks = res$callbacks
      search_space = res$search_space

      if (is.null(rush)) rush = rush::rsh()

      # create codomain from measure
      measures = assert_measures(as_measures(measures, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      archive = ArchiveAsyncTuning$new(
        search_space = search_space,
        codomain = codomain,
        rush = rush,
        internal_search_space = private$.internal_search_space
      )

      objective = ObjectiveTuningAsync$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks)

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
    #'  Optimal outcomes, e.g. the Pareto front.
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
    },
    #' @field internal_search_space ([paradox::ParamSet])\cr
    #'   The search space containing those parameters that are internally optimized by the [`mlr3::Learner`].
    internal_search_space = function(rhs) {
      assert_ro_binding(rhs)
      private$.internal_search_space
    }
  ),

  private = list(
    .internal_search_space = NULL,

    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextAsyncTuning$new(self, optimizer)
      self$objective$context = context
    }
  )
)
