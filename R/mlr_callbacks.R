#' @title Early Stopping Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.early_stopping
#'
#' @description
#' This [CallbackTuning] integrates early stopping into the hyperparameter tuning of an XGBoost learner.
#' Early stopping estimates the optimal number of trees (`nrounds`) for a given hyperparameter configuration.
#' Since early stopping is performed in each resampling iteration, there are several optimal `nrounds` values.
#' The callback writes the maximum value to the archive in the `max_nrounds` column.
#' In the best hyperparameter configuration (`instance$result_learner_param_vals`), the value of `nrounds` is replaced by `max_nrounds` and early stopping is deactivated.
#'
#' @details
#' Currently, the callback does not work with `GraphLearner`s from the package \CRANpkg{mlr3pipelines}.
#' The callback is compatible with the [AutoTuner].
#' The final model is fitted with the best hyperparameter configuration and `max_nrounds` i.e. early stopping is not performed.
#'
#' @section Resources:
#' * [gallery post](https://mlr-org.com/gallery/optimization/2022-11-04-early-stopping-with-xgboost/) on early stopping with XGBoost.
#'
#' @examples
#' clbk("mlr3tuning.early_stopping")
#' \donttest{
#' if (requireNamespace("mlr3learners") && requireNamespace("xgboost") ) {
#'   library(mlr3learners)
#'
#'   # activate early stopping on the test set and set search space
#'   learner = lrn("classif.xgboost",
#'     eta = to_tune(1e-02, 1e-1, logscale = TRUE),
#'     early_stopping_rounds = 5,
#'     nrounds = 100,
#'     early_stopping_set = "test")
#'
#'   # tune xgboost on the pima data set
#'   instance = tune(
#'     tuner = tnr("random_search"),
#'     task = tsk("pima"),
#'     learner = learner,
#'     resampling = rsmp("cv", folds = 3),
#'     measures = msr("classif.ce"),
#'     term_evals = 10,
#'     callbacks = clbk("mlr3tuning.early_stopping")
#'   )
#' }
#' }
NULL

load_callback_early_stopping = function() {
  callback_tuning("mlr3tuning.early_stopping",
    label = "Early Stopping Callback",
    man = "mlr3tuning::mlr3tuning.early_stopping",
    on_optimization_begin = function(callback, context) {
      learner = context$instance$objective$learner

      if (all(c("LearnerClassifXgboost", "LearnerRegrXgboost", "LearnerSurvXgboost") %nin% class(learner))) {
        stopf("%s is incompatible with %s", format(learner), format(callback))
      }

      if (is.null(learner$param_set$values$early_stopping_rounds)) {
        stop("Early stopping is not activated. Set `early_stopping_rounds` parameter.")
      }

      # store models temporary
      callback$state$store_models = context$instance$objective$store_models
      context$instance$objective$store_models = TRUE
    },

    on_eval_after_benchmark = function(callback, context) {
      callback$state$max_nrounds = map_dbl(context$benchmark_result$resample_results$resample_result, function(rr) {
          max(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) {
            state$model$best_iteration # GraphLearner state$model$xgboost$model$niter
          }))
      })
    },

    on_eval_before_archive = function(callback, context) {
      set(context$aggregated_performance, j = "max_nrounds", value = callback$state$max_nrounds)
      if (!callback$state$store_models) context$benchmark_result$discard(models = TRUE)
    },

    on_result = function(callback, context) {
      context$result$learner_param_vals[[1]]$early_stopping_rounds = NULL
      context$result$learner_param_vals[[1]]$nrounds = context$instance$archive$best()$max_nrounds
      context$result$learner_param_vals[[1]]$early_stopping_set = "none"
      context$instance$objective$store_models = callback$state$store_models
    }
  )
}

#' @title Backup Benchmark Result Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.backup
#'
#' @description
#' This [CallbackTuning] writes the [mlr3::BenchmarkResult] after each batch to disk.
#'
#' @examples
#' clbk("mlr3tuning.backup", path = "backup.rds")
#'
#' # tune classification tree on the pima data set
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.backup", path = tempfile(fileext = ".rds"))
#' )
NULL

load_callback_backup = function() {
  callback_tuning("mlr3tuning.backup",
    label = "Backup Benchmark Result Callback",
    man = "mlr3tuning::mlr3tuning.backup",
    on_optimization_begin = function(callback, context) {
      if (is.null(callback$state$path)) callback$state$path = "bmr.rds"
      assert_path_for_output(callback$state$path)
    },

    on_optimizer_after_eval = function(callback, context) {
      if (file.exists(callback$state$path)) unlink(callback$state$path)
      saveRDS(context$instance$archive$benchmark_result, callback$state$path)
    }
  )
}

#' @title Measure Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.measures
#'
#' @description
#' This [CallbackTuning] scores the hyperparameter configurations on additional measures while tuning.
#' Usually, the configurations can be scored on additional measures after tuning (see [ArchiveTuning]).
#' However, if the memory is not sufficient to store the [mlr3::BenchmarkResult], it is necessary to score the additional measures while tuning.
#' The measures are not taken into account by the tuner.
#'
#' @examples
#' clbk("mlr3tuning.measures")
#'
#' # additionally score the configurations on the accuracy measure
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.measures", measures = msr("classif.acc"))
#' )
#'
#' # score the configurations on the holdout set
#' task = tsk("pima")
#' splits = partition(task, ratio = 0.8)
#' task$row_roles$use = splits$train
#' task$row_roles$holdout = splits$test
#'
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#' learner$predict_sets = c("test", "holdout")
#'
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.measures", measures = msr("classif.ce",
#'     predict_sets = "holdout", id = "classif.ce_holdout"))
#' )
NULL

load_callback_measures = function() {
  callback_tuning("mlr3tuning.measures",
    label = "Additional Measures Callback",
    man = "mlr3tuning::mlr3tuning.measures",
    on_optimization_begin = function(callback, context) {
      callback$state$measures = assert_measures(as_measures(callback$state$measures, clone = TRUE))
      callback$state$ids = map_chr(callback$state$measures, "id")
      assert_names(callback$state$ids, type = "unique", .var.name = "measures")
      if (any(callback$state$ids %in% map_chr(context$instance$objective$measures, "id"))) {
        stopf("The measure id(s) '%s' are already used by the instance. Please pass the measures with a different id.", as_short_string(callback$state$ids))
      }
    },

    on_eval_before_archive = function(callback, context) {
      set(context$aggregated_performance, j = callback$state$ids, value = context$benchmark_result$aggregate(callback$state$measures)[, callback$state$ids, with = FALSE])
    }
  )
}
