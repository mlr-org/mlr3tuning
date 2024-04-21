#' @title Multi-Criteria Tuning with Rush
#'
#' @include TuningInstanceBatchSingleCrit.R ArchiveAsyncTuning.R
#'
#' @description
#' The [TuningInstanceAsyncMultiCrit] specifies a tuning problem for [Tuner]s.
#' Hyperparameter configurations are evaluated asynchronously with the `rush` package.
#' The function [ti()] creates a [TuningInstanceAsyncMultiCrit] and the function [tune()] creates an instance internally.
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
#' @template param_allow_hotstart
#' @template param_hotstart_threshold
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#' @template param_xdt
#' @template param_learner_param_vals
#' @template param_rush
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
      allow_hotstart = FALSE,
      hotstart_threshold = NULL,
      keep_hotstart_stack = FALSE,
      evaluate_default = FALSE,
      callbacks = list(),
      rush = NULL
      ) {
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

      if (is.null(rush)) rush = rsh()

      # create codomain from measure
      measures = assert_measures(as_measures(measures), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      archive = ArchiveAsyncTuning$new(
        search_space = search_space,
        codomain = codomain,
        rush = rush)

      objective = ObjectiveTuningAsync$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        allow_hotstart = allow_hotstart,
        hotstart_threshold = hotstart_threshold,
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
    #' Start workers with the `future` package.
    #'
    #' @template param_n_workers
    #' @template param_host
    #' @template param_heartbeat_period
    #' @template param_heartbeat_expire
    #' @template param_lgr_thresholds
    #' @template param_await_workers
    #' @template param_detect_lost_tasks
    #' @template param_freeze_archive
    start_workers = function(
      n_workers = NULL,
      host = "local",
      heartbeat_period = NULL,
      heartbeat_expire = NULL,
      lgr_thresholds = NULL,
      await_workers = TRUE,
      detect_lost_tasks = FALSE,
      freeze_archive = FALSE
      ) {
      super$start_workers(
        n_workers = n_workers,
        packages = c(self$objective$learner$packages, "mlr3tuning"),
        host = host,
        heartbeat_period = heartbeat_period,
        heartbeat_expire = heartbeat_expire,
        lgr_thresholds = lgr_thresholds,
        await_workers = await_workers,
        detect_lost_tasks = detect_lost_tasks,
        freeze_archive = freeze_archive)
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
    .evaluate_default = NULL,

    .assign_result = function(xdt, ydt, learner_param_vals = NULL) {
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
      super$.assign_result(xdt, ydt)
    }
  )
)
