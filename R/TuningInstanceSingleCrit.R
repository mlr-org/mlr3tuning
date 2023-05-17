#' @title Class for Single Criterion Tuning
#
#' @description
#' The [TuningInstanceSingleCrit] specifies a tuning problem for [Tuners][Tuner].
#' The function [ti()] creates a [TuningInstanceSingleCrit] and the function [tune()] creates an instance internally.
#'
#' @details
#' The instance contains an [ObjectiveTuning] object that encodes the black box objective function a [Tuner] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_batch()`).
#' This operation is usually done by the [Tuner].
#' Evaluations of hyperparameter configurations are performed in batches by calling [mlr3::benchmark()] internally.
#' The evaluated hyperparameter configurations are stored in the [Archive][ArchiveTuning] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The tuner is also supposed to store its final result, consisting of a  selected hyperparameter configuration and associated estimated performance values, by calling the method `instance$assign_result`.
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
#' @inheritSection ArchiveTuning Analysis
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * Getting started with [hyperparameter optimization](https://mlr3book.mlr-org.com/optimization.html).
#'  * [Tune](https://mlr3book.mlr-org.com/optimization.html#sec-tuning-instance) a simple classification tree on the Palmer Penguins data set.
#'  * Learn about [tuning spaces](https://mlr3book.mlr-org.com/technical.html#sec-tuning-space).
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#'  * Learn more advanced methods with the [practical tuning series](https://mlr-org.com/gallery/series/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/).
#'  * Simultaneously optimize hyperparameters and use [early stopping](https://mlr-org.com/gallery/optimization/2022-11-04-early-stopping-with-xgboost/) with XGBoost.
#'  * Make us of proven [search space](https://mlr-org.com/gallery/optimization/2021-07-06-introduction-to-mlr3tuningspaces/).
#'  * Learn about [hotstarting](https://mlr-org.com/gallery/optimization/2023-01-16-hotstart/) models.
#'  * Run the [default hyperparameter configuration](https://mlr-org.com/gallery/optimization/2023-01-31-default-configuration/) of learners as a baseline.
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
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_evaluate_default
#' @template param_callbacks
#' @template param_xdt
#' @template param_learner_param_vals
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
TuningInstanceSingleCrit = R6Class("TuningInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,

  public = list(

    #' @field promises (`list()`)\cr
    #' List of promises.
    promises = NULL,

    #' @field instance_id (`character(1)`)\cr
    #' Unique identifier of the instance.
    instance_id = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measure = NULL, terminator, search_space = NULL, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, evaluate_default = FALSE, callbacks = list()) {
      self$instance_id = uuid::UUIDgenerate()

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
      measures = assert_measures(as_measures(measure, task_type = task$task_type, clone = TRUE), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      archive = ArchiveTuning$new(search_space, codomain, check_values)
      objective = ObjectiveTuning$new(task, learner, resampling, measures, store_benchmark_result, store_models, check_values, allow_hotstart, keep_hotstart_stack, archive, callbacks)

      super$initialize(objective, search_space, terminator, callbacks = callbacks)
      # super class of instance initializes default archive, overwrite with tuning archive
      self$archive = archive
    },

    #' @description
    #' Asynchronously evaluates the objective function at the given points.
    #'
    #' @param xdt (`data.table::data.table()`)\cr
    #' x values as `data.table()` with one point per row.
    #' Contains the value in the *search space* of the [OptimInstance] object.
    #' Can contain additional columns for extra information.
    eval_async = function(xdt) {
      private$.xdt = xdt
      call_back("on_optimizer_before_eval", self$callbacks, private$.context)
      # update progressor
      if (!is.null(self$progressor)) self$progressor$update(self$terminator, self$archive)

      if (self$is_terminated) stop(bbotk:::terminated_error(self))
      assert_data_table(xdt)
      assert_names(colnames(xdt), must.include = self$search_space$ids())

      lg$info("Sending %i configuration(s) to %i workers", max(1, nrow(xdt)), self$archive$n_workers)

      xss = transform_xdt_to_xss(private$.xdt, self$search_space)
      self$archive$write_xdt_xss(private$.xdt, xss)

      # if futures resolve, an error occurred on the workers
      if (any(future::resolved(self$promises))) stop("Error on the workers.")
    },

    #' @description
    #' Starts the workers.
    start_workers = function()  {
      objective = self$objective
      archive = self$archive
      self$promises = replicate(self$archive$n_workers,
        future::future(mlr3tuning::wrapper_async(objective, archive),
        seed = TRUE,
        packages = "mlr3tuning"))
    },

    #' @description
    #' Terminates the workers.
    terminate_workers = function() {
      r = self$archive$connector
      r$SET(get_private(self$archive)$.get_key("terminated"), TRUE)

      # try to terminate workers for 10 seconds
      time = Sys.time()
      while (difftime(Sys.time(), time, units = "secs") < 10) {
        if (all(future::resolved(self$promises))) break
      }

      if (all(future::resolved(self$promises))) {
        lg$info("Terminating %i workers", self$archive$n_workers)
      } else {
        warning("Workers could not be terminated.")
      }
    },

    #' @description
    #' The [Tuner] object writes the best found point and estimated performance value here.
    #' For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #'   Optimal outcome.
    assign_result = function(xdt, y, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      assert_list(learner_param_vals, null.ok = TRUE, names = "named")

      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$values
      }
      opt_x = unlist(transform_xdt_to_xss(xdt, self$search_space), recursive = FALSE)
      learner_param_vals = insert_named(learner_param_vals, opt_x)

      # maintain list column
      if (length(learner_param_vals) < 2 | !nrow(xdt)) learner_param_vals = list(learner_param_vals)

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
    .evaluate_default = NULL
  )
)

#' @title Wrapper
#'
#' @description
#' Wrapper function for asynchronous evaluation of the objective function.
#'
#' @param objective ([ObjectiveTuning])\cr
#'  Objective function.
#' @param archive ([ArchiveTuning])\cr
#' Archive of evaluated points.
#'
#' @export
wrapper_async = function(objective, archive) {
  while (!archive$terminated) {
    xs = archive$pop_xs()
    if (!is.null(xs)) {
      ys = objective$eval(xs$xs)
      archive$write_ys(xs$key, ys)
    }
  }
}
