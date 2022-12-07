#' @title Class for Automatic Tuning
#'
#' @description
#' The [AutoTuner] wraps a [mlr3::Learner] and augments it with an automatic tuning process for a given set of hyperparameters.
#' The [auto_tuner()] function creates an [AutoTuner] object.
#'
#' @details
#' The [AutoTuner] is a [mlr3::Learner] which wraps another [mlr3::Learner] and performs the following steps during `$train()`:
#'
#' 1. The hyperparameters of the wrapped (inner) learner are trained on the training data via resampling.
#'    The tuning can be specified by providing a [Tuner], a [bbotk::Terminator], a search space as [paradox::ParamSet], a [mlr3::Resampling] and a [mlr3::Measure].
#' 2. The best found hyperparameter configuration is set as hyperparameters for the wrapped (inner) learner stored in `at$learner`.
#'    Access the tuned hyperparameters via `at$tuning_result`.
#' 3. A final model is fit on the complete training data using the now parametrized wrapped learner.
#'    The respective model is available via field `at$learner$model`.
#'
#' During `$predict()` the `AutoTuner` just calls the predict method of the wrapped (inner) learner.
#' A set timeout is disabled while fitting the final model.
#'
#' @section Resources:
#' * [book chapter](https://mlr3book.mlr-org.com/optimization.html#sec-autotuner) on automatic tuning.
#' * [book chapter](https://mlr3book.mlr-org.com/optimization.html#sec-nested-resampling) on nested resampling.
#' * [gallery post](https://mlr-org.com/gallery/series/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/) on tuning and nested resampling.
#'
#' @section Nested Resampling:
#' Nested resampling can be performed by passing an [AutoTuner] object to [mlr3::resample()] or [mlr3::benchmark()].
#' To access the inner resampling results, set `store_tuning_instance = TRUE` and execute [mlr3::resample()] or [mlr3::benchmark()] with `store_models = TRUE` (see examples).
#' The [mlr3::Resampling] passed to the [AutoTuner] is meant to be the inner resampling, operating on the training set of an arbitrary outer resampling.
#' For this reason it is not feasible to pass an instantiated [mlr3::Resampling] here.
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_search_space
#' @template param_store_tuning_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#'
#' @export
#' @examples
#' # Automatic Tuning
#'
#' # split to train and external set
#' task = tsk("penguins")
#' split = partition(task, ratio = 0.8)
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE)
#' )
#'
#' # create auto tuner
#' at = auto_tuner(
#'   method = tnr("random_search"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # tune hyperparameters and fit final model
#' at$train(task, row_ids = split$train)
#'
#' # predict with final model
#' at$predict(task, row_ids = split$test)
#'
#' # show tuning result
#' at$tuning_result
#'
#' # model slot contains trained learner and tuning instance
#' at$model
#'
#' # shortcut trained learner
#' at$learner
#'
#' # shortcut tuning instance
#' at$tuning_instance
#'
#'
#' # Nested Resampling
#'
#' at = auto_tuner(
#'   method = tnr("random_search"),
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 3)
#' rr = resample(task, at, resampling_outer, store_models = TRUE)
#'
#' # retrieve inner tuning results.
#' extract_inner_tuning_results(rr)
#'
#' # performance scores estimated on the outer resampling
#' rr$score()
#'
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
AutoTuner = R6Class("AutoTuner",
  inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the [TuningInstanceSingleCrit].
    instance_args = NULL,

    #' @field tuner ([Tuner])\cr
    #' Optimization algorithm.
    tuner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param tuner ([Tuner])\cr
    #'   Optimization algorithm.
    initialize = function(learner, resampling, measure = NULL, terminator, tuner, search_space = NULL, store_tuning_instance = TRUE, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, callbacks = list()) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token")) > 0) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }

      ia = list()
      ia$learner = learner
      ia$resampling = assert_resampling(resampling, instantiated = FALSE)$clone()
      if (!is.null(measure)) ia$measure = assert_measure(as_measure(measure), learner = learner)
      if (!is.null(search_space)) ia$search_space = assert_param_set(as_search_space(search_space))$clone()
      ia$terminator = assert_terminator(terminator)$clone()

      ia$store_models = assert_flag(store_models)
      ia$store_benchmark_result = assert_flag(store_benchmark_result) || ia$store_models
      private$.store_tuning_instance = assert_flag(store_tuning_instance) || ia$store_benchmark_result

      ia$check_values = assert_flag(check_values)
      ia$callbacks = assert_callbacks(as_callbacks(callbacks))
      self$instance_args = ia
      self$tuner = assert_tuner(tuner)$clone()

      super$initialize(
        id = paste0(learner$id, ".tuned"),
        task_type = learner$task_type,
        packages = c("mlr3tuning", learner$packages),
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        properties = learner$properties
      )

      self$predict_type = learner$predict_type
      self$predict_sets = learner$predict_sets
    },

    #' @description
    #' Extracts the base learner from nested learner objects like `GraphLearner` in \CRANpkg{mlr3pipelines}.
    #' If `recursive = 0`, the (tuned) learner is returned.
    #'
    #' @param recursive (`integer(1)`)\cr
    #'   Depth of recursion for multiple nested objects.
    #'
    #' @return [Learner].
    base_learner = function(recursive = Inf) {
      if (recursive == 0L) self$learner else self$learner$base_learner(recursive - 1L)
    },

    #' @description
    #' The importance scores of the final model.
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if ("importance" %nin% self$instance_args$learner$properties) {
        stopf("Learner ''%s' cannot calculate important scores.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$importance()
      } else {
        self$model$learner$importance()
      }
    },

    #' @description
    #' The selected features of the final model.
    #'
    #' @return `character()`.
    selected_features = function() {
      if ("selected_features" %nin% self$instance_args$learner$properties) {
        stopf("Learner ''%s' cannot select features.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$selected_features()
      } else {
        self$model$learner$selected_features()
      }
    },

    #' @description
    #' The out-of-bag error of the final model.
    #'
    #' @return `numeric(1)`.
    oob_error = function() {
      if ("oob_error" %nin% self$instance_args$learner$properties) {
        stopf("Learner '%s' cannot calculate the out-of-bag error.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$oob_error()
      } else {
        self$model$learner$oob_error()
      }
    },

    #' @description
    #' The log-likelihood of the final model.
    #'
    #' @return `logLik`.
    loglik = function() {
      if ("loglik" %nin% self$instance_args$learner$properties) {
        stopf("Learner '%s' cannot calculate the log-likelihood.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$loglik()
      } else {
        self$model$learner$loglik()
      }
    },

    #' Printer.
    #' @param ... (ignored).
    print = function() {
      search_space = if (is.null(self$instance_args$search_space)) {
        self$instance_args$learner$param_set$search_space()
      } else {
        self$instance_args$search_space
      }
      catf(format(self))
      catf(str_indent("* Model:", if (is.null(self$model)) "-" else class(self$model)[1L]))
      catf("* Search Space:")
      print(search_space)
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Predict Type:", self$predict_type))
      catf(str_indent("* Feature Types:", self$feature_types))
      catf(str_indent("* Properties:", self$properties))
      w = self$warnings
      e = self$errors
      if (length(w)) {
        catf(str_indent("* Warnings:", w))
      }
      if (length(e)) {
        catf(str_indent("* Errors:", e))
      }
    }
  ),

  active = list(

    #' @field archive [ArchiveTuning]\cr
    #' Archive of the [TuningInstanceSingleCrit].
    archive = function() self$tuning_instance$archive,

    #' @field learner ([mlr3::Learner])\cr
    #' Trained learner
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },

    #' @field tuning_instance ([TuningInstanceSingleCrit])\cr
    #' Internally created tuning instance with all intermediate results.
    tuning_instance = function() self$model$tuning_instance,

    #' @field tuning_result ([data.table::data.table])\cr
    #' Short-cut to `result` from [TuningInstanceSingleCrit].
    tuning_result = function() self$tuning_instance$result,

    #' @field predict_type (`character(1)`)\cr
    #' Stores the currently active predict type, e.g. `"response"`.
    #' Must be an element of `$predict_types`.
    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      if (rhs %nin% self$predict_types) {
        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }

      # Catches 'Error: Field/Binding is read-only' bug
      tryCatch({
        self$model$learner$predict_type = rhs
      }, error = function(cond){})

      private$.predict_type = rhs
    },

    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    hash = function(rhs) {
      assert_ro_binding(rhs)
      calculate_hash(class(self), self$id, self$param_set$values, private$.predict_type, self$fallback$hash, self$instance_args,
        private$.store_tuning_instance)
    }
  ),

  private = list(
    .train = function(task) {
      # construct instance from args; then tune
      ia = self$instance_args
      ia$task = task
      instance = do.call(TuningInstanceSingleCrit$new, ia)
      self$tuner$optimize(instance)

      # get learner, set params to optimal, then train we REALLY need to clone
      # here we write to the object and this would change instance_args
      learner = ia$learner$clone(deep = TRUE)
      learner$param_set$values = instance$result_learner_param_vals
      # disable timeout to allow train on full data set without time limit
      # timeout during tuning is not affected
      learner$timeout = c(train = Inf, predict = Inf)
      learner$train(task)

      # the return model is a list of "learner" and "tuning_instance"
      result_model = list(learner = learner)
      if (private$.store_tuning_instance) result_model$tuning_instance = instance
      result_model
    },

    .predict = function(task) {
      self$model$learner$predict(task)
    },

    .store_tuning_instance = NULL
  )
)
