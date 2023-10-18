#' @title Class for Multi Criteria Tuning
#'
#' @include TuningInstanceSingleCrit.R ArchiveTuning.R
#'
#' @description
#' The [TuningInstanceMultiCrit] specifies a tuning problem for [Tuners][Tuner].
#' The function [ti()] creates a [TuningInstanceMultiCrit] and the function [tune()] creates an instance internally.
#'
#' @inherit TuningInstanceSingleCrit details
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * Learn about [multi-objective optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html#sec-multi-metrics-tuning).
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#' @inheritSection ArchiveTuning Analysis
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
#' @template param_start_workers
#' @template param_lgr_thresholds
#' @template param_freeze_archive
#'
#' @export
#' @examples
#' # Hyperparameter optimization on the Palmer Penguins data set
#' task = tsk("penguins")
#'
#' # Load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE)
#' )
#'
#' # Construct tuning instance
#' instance = ti(
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msrs(c("classif.ce", "time_train")),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Choose optimization algorithm
#' tuner = tnr("random_search", batch_size = 2)
#'
#' # Run tuning
#' tuner$optimize(instance)
#'
#' # Optimal hyperparameter configurations
#' instance$result
#'
#' # Inspect all evaluated configurations
#' as.data.table(instance$archive)
TuningInstanceMultiCrit = R6Class("TuningInstanceMultiCrit",
  inherit = OptimInstanceMultiCrit,
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
      rush = NULL,
      start_workers = TRUE,
      lgr_thresholds = NULL,
      freeze_archive = FALSE) {

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
      measures = assert_measures(as_measures(measures), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      if (is.null(rush)) {
        archive = ArchiveTuning$new(
          search_space = search_space,
          codomain = codomain,
          check_values = check_values
        )
        objective = ObjectiveTuning$new(
          task = task,
          learner = learner,
          resampling = resampling,
          measures = measures,
          store_benchmark_result = store_benchmark_result,
          store_models = store_models,
          check_values =  check_values,
          allow_hotstart = allow_hotstart,
          hotstart_threshold = hotstart_threshold,
          keep_hotstart_stack = keep_hotstart_stack,
          archive = archive,
          callbacks = callbacks)
      } else {
        archive = ArchiveRushTuning$new(
          search_space = search_space,
          codomain = codomain,
          check_values = check_values,
          rush = rush)
        objective = ObjectiveRushTuning$new(
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
      }

      super$initialize(
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        callbacks = callbacks,
        rush = rush,
        lgr_thresholds = lgr_thresholds,
        freeze_archive = freeze_archive)

      # super class of instance initializes default archive, overwrite with tuning archive
      self$archive = archive
    },

    #' @description
    #' The [Tuner] object writes the best found points and estimated performance values here.
    #' For internal use.
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
