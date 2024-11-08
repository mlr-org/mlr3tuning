#' @title Class for Single Criterion Tuning
#
#' @description
#' The [TuningInstanceBatchSingleCrit] specifies a tuning problem for a [Tuner].
#' The function [ti()] creates a [TuningInstanceBatchSingleCrit] and the function [tune()] creates an instance internally.
#'
#' @details
#' The instance contains an [ObjectiveTuningBatch] object that encodes the black box objective function a [Tuner] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_batch()`).
#' This operation is usually done by the [Tuner].
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' The evaluated hyperparameter configurations are stored in the [ArchiveBatchTuning] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The tuner is also supposed to store its final result, consisting of a selected hyperparameter configuration and associated estimated performance values, by calling the method `instance$assign_result`.
#'
#' @section Default Measures:
#' If no measure is passed, the default measure is used.
#' The default measure depends on the task type.
#'
#' | Task           | Default Measure     | Package               |
#' |----------------|---------------------|-----------------------|
#' | `"classif"`    | `"classif.ce"`      | \CRANpkg{mlr3}        |
#' | `"regr"`       | `"regr.mse"`        | \CRANpkg{mlr3}        |
#' | `"surv"`       | `"surv.cindex"`     | \CRANpkg{mlr3proba}   |
#' | `"dens"`       | `"dens.logloss"`    | \CRANpkg{mlr3proba}   |
#' | `"classif_st"` | `"classif.ce"`      | \CRANpkg{mlr3spatial} |
#' | `"regr_st"`    | `"regr.mse"`        | \CRANpkg{mlr3spatial} |
#' | `"clust"`      | `"clust.dunn"`      | \CRANpkg{mlr3cluster} |
#'
#' @inheritSection ArchiveBatchTuning Analysis
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' * Getting started with [hyperparameter optimization](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html).
#' * An overview of all tuners can be found on our [website](https://mlr-org.com/tuners.html).
#' * [Tune](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-model-tuning) a support vector machine on the Sonar data set.
#' * Learn about [tuning spaces](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-defining-search-spaces).
#' * Estimate the model performance with [nested resampling](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-nested-resampling).
#' * Learn about [multi-objective optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html#sec-multi-metrics-tuning).
#' * Simultaneously optimize hyperparameters and use [early stopping](https://mlr3book.mlr-org.com/chapters/chapter15/predsets_valid_inttune.html) with XGBoost.
#' * [Automate](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-autotuner) the tuning.
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#' * Learn more advanced methods with the [Practical Tuning Series](https://mlr-org.com/gallery/series/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/).
#' * Learn about [hotstarting](https://mlr-org.com/gallery/optimization/2023-01-16-hotstart/) models.
#' * Run the [default hyperparameter configuration](https://mlr-org.com/gallery/optimization/2023-01-31-default-configuration/) of learners as a baseline.
#' * Use the [Hyperband](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/) optimizer with different budget parameters.
#'
#' The [cheatsheet](https://cheatsheets.mlr-org.com/mlr3tuning.pdf) summarizes the most important functions of mlr3tuning.
#'
#' @section Extension Packages:
#'
#' mlr3tuning is extended by the following packages.
#'
#'  * [mlr3tuningspaces](https://github.com/mlr-org/mlr3tuningspaces) is a collection of search spaces from scientific articles for commonly used learners.
#'  * [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the Hyperband and Successive Halving algorithm.
#'  * [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian optimization methods.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_search_space
#' @template param_internal_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#'
#' @template param_xdt
#' @template param_learner_param_vals
#' @template param_internal_tuned_values
#' @template param_extra
#'
#' @template field_internal_search_space
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
#'   measures = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Choose optimization algorithm
#' tuner = tnr("random_search", batch_size = 2)
#'
#' # Run tuning
#' tuner$optimize(instance)
#'
#' # Set optimal hyperparameter configuration to learner
#' learner$param_set$values = instance$result_learner_param_vals
#'
#' # Train the learner on the full data set
#' learner$train(task)
#'
#' # Inspect all evaluated configurations
#' as.data.table(instance$archive)
TuningInstanceBatchSingleCrit = R6Class("TuningInstanceBatchSingleCrit",
  inherit = OptimInstanceBatchSingleCrit,

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
      internal_search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL
      ) {
      learner = assert_learner(as_learner(learner, clone = TRUE))
      callbacks = assert_batch_tuning_callbacks(as_callbacks(callbacks))

      # tune token and search space
      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token"))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }

      search_space_from_tokens = is.null(search_space)

      # convert tune token to search space
      search_space = if (is.null(search_space)) {
        learner$param_set$search_space()
      } else {
        as_search_space(search_space)
      }

      # get ids of primary and internal hyperparameters
      sids = search_space$ids()
      internal_tune_ids = search_space$ids(any_tags = "internal_tuning")

      # get internal search space
      self$internal_search_space = if (is.null(internal_search_space)) {
        # We DO NOT subset the search space because there we might keep an extra_trafo which is not allowed
        # for the internal tuning search space
        if (length(internal_tune_ids)) {
          if (search_space_from_tokens) {
            learner$param_set$subset(internal_tune_ids)$search_space()
          } else {
            search_space$subset(internal_tune_ids)
          }
        }
      } else {
        if (length(internal_tune_ids)) {
          stopf("Either tag parameters in the `search_space` with 'internal_tuning' OR provide an `internal_search_space`.")
        }
        as_search_space(internal_search_space)
      }

      # subset search space to primary hyperparameters
      if (length(internal_tune_ids)) {
        search_space = search_space$subset(setdiff(sids, internal_tune_ids))
      }

      # set learner parameter values
      if (search_space_from_tokens) {
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      }

      if (!is.null(self$internal_search_space) && self$internal_search_space$has_trafo) {
        stopf("Internal tuning and parameter transformations are currently not supported.
          If you manually provided a search space that has a trafo and parameters tagged with 'internal_tuning',
          please pass the latter separately via the argument `internal_search_space`.")
      }

      # set internal search space
      if (!is.null(self$internal_search_space)) {
        # the learner dictates how to interpret the to_tune(..., inner)
        learner$param_set$set_values(.values = learner$param_set$convert_internal_search_space(self$internal_search_space))
      }

      # create codomain from measure
      measures = assert_measures(as_measures(measure, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      archive = ArchiveBatchTuning$new(
        search_space = search_space,
        codomain = codomain,
        check_values = check_values,
        internal_search_space = self$internal_search_space
      )

      objective = ObjectiveTuningBatch$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        archive = archive,
        callbacks = callbacks,
        internal_search_space = self$internal_search_space)

      super$initialize(
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        callbacks = callbacks,
        archive = archive)
    },


    #' @description
    #' The [Tuner] object writes the best found point and estimated performance value here.
    #' For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    #' @param xydt (`data.table::data.table()`)\cr
    #' Point, outcome, and additional information (Deprecated).
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, y, learner_param_vals = NULL, extra = NULL, xydt = NULL, ...) {
      # workaround
      extra = extra %??% xydt

      # assign for callbacks
      private$.result_xdt = xdt
      private$.result_y = y
      private$.result_learner_param_vals = learner_param_vals
      private$.result_extra = extra

      call_back("on_tuning_result_begin", self$objective$callbacks, self$objective$context)

      # set the column with the learner param_vals that were not optimized over but set implicitly
      assert_list(private$.result_learner_param_vals, null.ok = TRUE, names = "named")

      # extract internal tuned values
      if ("internal_tuned_values" %in% names(private$.result_extra)) {
        set(private$.result_xdt, j = "internal_tuned_values", value = list(private$.result_extra[["internal_tuned_values"]]))
      }

      # learner param values
      if (is.null(private$.result_learner_param_vals)) {
        private$.result_learner_param_vals = self$objective$learner$param_set$values
      }
      opt_x = unlist(transform_xdt_to_xss(private$.result_xdt, self$search_space), recursive = FALSE)
      private$.result_learner_param_vals = insert_named(private$.result_learner_param_vals, opt_x)

      # disable internal tuning
      if (!is.null(private$.result_xdt$internal_tuned_values)) {
        learner = self$objective$learner$clone(deep = TRUE)
        private$.result_learner_param_vals = insert_named(private$.result_learner_param_vals, private$.result_xdt$internal_tuned_values[[1]])
        learner$param_set$set_values(.values = private$.result_learner_param_vals)
        learner$param_set$disable_internal_tuning(self$internal_search_space$ids())
        private$.result_learner_param_vals = learner$param_set$values
      }

      # maintain list column
      if (length(private$.result_learner_param_vals) < 2 | !nrow(private$.result_xdt)) private$.result_learner_param_vals = list(private$.result_learner_param_vals)

      set(private$.result_xdt, j = "learner_param_vals", value = list(private$.result_learner_param_vals))
      super$assign_result(private$.result_xdt, private$.result_y)
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
    # intermediate objects
    .result_learner_param_vals = NULL,

    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextBatchTuning$new(self, optimizer)
      self$objective$context = context
    }
  )
)

