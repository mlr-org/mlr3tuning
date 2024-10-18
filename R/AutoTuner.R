#' @title Class for Automatic Tuning
#'
#' @description
#'
#' The [AutoTuner] wraps a [mlr3::Learner] and augments it with an automatic tuning process for a given set of hyperparameters.
#' The [auto_tuner()] function creates an [AutoTuner] object.
#'
#' @section Validation:
#' The `AutoTuner` itself does **not** have the `"validation"` property.
#' To enable validation during the tuning, set the `$validate` field of the tuned learner.
#' This is also possible via `set_validate()`.
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
#' @inheritSection TuningInstanceBatchSingleCrit Default Measures
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * [Automate](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-autotuner) the tuning.
#'  * Estimate the model performance with [nested resampling](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-nested-resampling).
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#' @section Nested Resampling:
#' Nested resampling is performed by passing an [AutoTuner] to [mlr3::resample()] or [mlr3::benchmark()].
#' To access the inner resampling results, set `store_tuning_instance = TRUE` and execute [mlr3::resample()] or [mlr3::benchmark()] with `store_models = TRUE` (see examples).
#' The [mlr3::Resampling] passed to the [AutoTuner] is meant to be the inner resampling, operating on the training set of an arbitrary outer resampling.
#' For this reason, the inner resampling should be not instantiated.
#' If an instantiated resampling is passed, the [AutoTuner] fails when a row id of the inner resampling is not present in the training set of the outer resampling.
#'
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_search_space
#' @template param_internal_search_space
#' @template param_store_tuning_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_rush
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
#'   tuner = tnr("random_search"),
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
#'   tuner = tnr("random_search"),
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
    #' All arguments from construction to create the [TuningInstanceBatchSingleCrit].
    instance_args = NULL,

    #' @field tuner ([Tuner])\cr
    #' Optimization algorithm.
    tuner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param tuner ([Tuner])\cr
    #'   Optimization algorithm.
    initialize = function(
      tuner,
      learner,
      resampling,
      measure = NULL,
      terminator,
      search_space = NULL,
      internal_search_space = NULL,
      store_tuning_instance = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL
      ) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token")) > 0) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }

      ia = list()
      self$tuner = assert_tuner(tuner)
      ia$learner = learner
      ia$resampling = assert_resampling(resampling)$clone()
      if (!is.null(measure)) ia$measure = assert_measure(as_measure(measure), learner = learner)
      if (!is.null(search_space)) ia$search_space = assert_param_set(as_search_space(search_space))$clone()
      if (!is.null(internal_search_space)) ia$search_space = assert_param_set(as_search_space(internal_search_space))$clone()
      ia$terminator = assert_terminator(terminator)$clone()

      ia$store_models = assert_flag(store_models)
      ia$store_benchmark_result = assert_flag(store_benchmark_result) || ia$store_models
      private$.store_tuning_instance = assert_flag(store_tuning_instance) || ia$store_benchmark_result

      ia$check_values = assert_flag(check_values)
      ia$callbacks = assert_callbacks(as_callbacks(callbacks))
      if (!is.null(rush)) ia$rush = assert_class(rush, "Rush")
      self$instance_args = ia

      super$initialize(
        id = paste0(learner$id, ".tuned"),
        task_type = learner$task_type,
        packages = c("mlr3tuning", learner$packages),
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        properties = setdiff(learner$properties, c("internal_tuning", "validation"))
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
    #' @return [mlr3::Learner].
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
      super$print()
      search_space = if (is.null(self$instance_args$search_space)) {
        self$instance_args$learner$param_set$search_space()
      } else {
        self$instance_args$search_space
      }
      catf("* Search Space:")
      print(as.data.table(search_space)[, c("id", "class", "lower", "upper", "nlevels"), with = FALSE])
    },

    #' @description
    #' Marshal the learner.
    #' @param ... (any)\cr
    #'   Additional parameters.
    #' @return self
    marshal = function(...) {
      learner_marshal(.learner = self, ...)
    },
    #' @description
    #' Unmarshal the learner.
    #' @param ... (any)\cr
    #'   Additional parameters.
    #' @return self
    unmarshal = function(...) {
      learner_unmarshal(.learner = self, ...)
    },
    #' @description
    #' Whether the learner is marshaled.
    marshaled = function() {
      learner_marshaled(self)
    }
  ),

  active = list(
    #' @field internal_valid_scores
    #' Retrieves the inner validation scores as a named `list()`.
    #' Returns `NULL` if learner is not trained yet.
    internal_valid_scores = function() {
      self$state$internal_valid_scores
    },

    #' @field archive [ArchiveBatchTuning]\cr
    #' Archive of the [TuningInstanceBatchSingleCrit].
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

    #' @field tuning_instance ([TuningInstanceAsyncSingleCrit] | [TuningInstanceBatchSingleCrit])\cr
    #' Internally created tuning instance with all intermediate results.
    tuning_instance = function() self$model$tuning_instance,

    #' @field tuning_result ([data.table::data.table])\cr
    #' Short-cut to `result` from  tuning instance.
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
      calculate_hash(class(self), self$id, self$param_set$values, private$.predict_type, self$fallback$hash, self$parallel_predict, self$tuner, self$instance_args, private$.store_tuning_instance)
    },

    #' @field phash (`character(1)`)\cr
    #' Hash (unique identifier) for this partial object, excluding some components which are varied systematically during tuning (parameter values) or feature selection (feature names).
    phash = function(rhs) {
      assert_ro_binding(rhs)
      self$hash
    }
  ),

  private = list(
    .train = function(task) {
      # construct instance from args; then tune
      ia = self$instance_args
      ia$task = task

      learner = ia$learner$clone(deep = TRUE)

      # check if task contains all row ids required for instantiated resampling
      if (ia$resampling$is_instantiated) {
        imap(ia$resampling$instance$train, function(x, i) {
          if (!test_subset(x, task$row_ids)) {
            stopf("Train set %i of inner resampling '%s' contains row ids not present in task '%s': {%s}", i, ia$resampling$id, task$id, paste(setdiff(x, task$row_ids), collapse = ", "))
          }
        })

        imap(ia$resampling$instance$test, function(x, i) {
          if (!test_subset(x, task$row_ids)) {
            stopf("Test set %i of inner resampling '%s' contains row ids not present in task '%s': {%s}", i, ia$resampling$id, task$id, paste(setdiff(x, task$row_ids), collapse = ", "))
          }
        })
      }

      TuningInstance = if (inherits(self$tuner, "TunerBatch")) TuningInstanceBatchSingleCrit else TuningInstanceAsyncSingleCrit
      instance = do.call(TuningInstance$new, ia)
      self$tuner$optimize(instance)

      # now we reset the validation task and clear the exit handlers

      # in the case of internal tuning, the result_learner_param_vals already contains the aggregated internally
      learner$param_set$values = instance$result_learner_param_vals
      # disable timeout to allow train on full data set without time limit
      # timeout during tuning is not affected
      learner$timeout = c(train = Inf, predict = Inf)
      if ("validation" %in% learner$properties) {
        set_validate(learner, validate = NULL)
      }
      learner$train(task)

      # the return model is a list of "learner" and "tuning_instance"
      result_model = list(learner = learner)
      if (private$.store_tuning_instance) result_model$tuning_instance = instance
      structure(result_model, class = c("auto_tuner_model", "list"))
    },
    .predict = function(task) {
      self$model$learner$predict(task)
    },
    .extract_internal_valid_scores = function() {
      self$model$learner$internal_valid_scores
    },
    .store_tuning_instance = NULL
  )
)

#' @export
marshal_model.auto_tuner_model = function(model, inplace = FALSE, ...) {
  if (inplace) {
    model$learner$model = marshal_model(model$learner$model, inplace = TRUE)
    x = structure(list(
      marshaled = model,
      packages = "mlr3tuning"
    ), class = c("auto_tuner_model_marshaled", "list_marshaled", "marshaled"))
    return(x)
  }
  # we clone the learner without its model
  learner = model$learner
  learner_model = learner$model
  on.exit({learner$model = learner_model}, add = TRUE)
  learner$model = NULL
  learner_clone = learner$clone(deep = TRUE)
  learner_clone$model = marshal_model(learner_model, inplace = FALSE)

  marshaled = list(learner = learner_clone)
  # note that we don't clone the tuning instance even when inplace is FALSE
  # For our use-case, this is not necessary and would cause unnecessary overhead in the the mlr3 workhorse function
  marshaled$tuning_instance = model$tuning_instance

  structure(list(
    marshaled = marshaled
    ), class = c("auto_tuner_model_marshaled", "list_marshaled", "marshaled"))
}


#' @export
unmarshal_model.auto_tuner_model_marshaled = function(model, inplace = FALSE, ...) {
  if (inplace) {
    at_model = model$marshaled
    at_model$learner$model = unmarshal_model(at_model$learner$model, inplace = TRUE)
    return(at_model)
  }

  at_model = model$marshaled

  prev_learner = at_model$learner
  prev_model = prev_learner$model
  prev_learner$model = NULL
  on.exit({prev_learner$model = prev_model}, add = TRUE)

  at_model$learner = prev_learner$clone(deep = TRUE)
  at_model$learner$model = unmarshal_model(prev_model, inplace = FALSE)

  return(structure(at_model, class = c("auto_tuner_model", "list")))
}

#' @title Configure Validation for AutoTuner
#'
#' @description
#' Configure validation data for the learner that is tuned by the `AutoTuner`.
#'
#' @param learner ([`AutoTuner`])\cr
#'   The autotuner for which to enable validation.
#' @param validate (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)\cr
#'   How to configure the validation during the hyperparameter tuning.
#' @param ... (any)\cr
#'   Passed when calling `set_validate()` on the wrapped leaerner.
#' @export
#' @examples
#' at = auto_tuner(
#'   tuner = tnr("random_search"),
#'   learner = lrn("classif.debug", early_stopping = TRUE,
#'     iter = to_tune(upper = 1000L, internal = TRUE), validate = 0.2),
#'   resampling = rsmp("holdout")
#' )
#' # use the test set as validation data during tuning
#' set_validate(at, validate = "test")
#' at$learner$validate
set_validate.AutoTuner = function(learner, validate, ...) {
  set_validate(learner$learner, validate = validate, ...)
  invisible(learner)
}
