#' @title Early Stopping Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.early_stopping
#'
#' @description
#' When tuning an XGBoost learner, early stopping can be used to find the optimal number of trees.
#'
#' @details
#' This [Callback] extracts the maximum number of trees found during resampling.
#' The maximum number of trees are added to the [ArchiveTuning] in the column `"max_nrounds"`.
#'
#' @examples
#' clbk("mlr3tuning.early_stopping")
NULL

load_callback_early_stopping = function() {
  callback_tuning("mlr3tuning.early_stopping",
    label = "Early Stopping Callback",
    man = "mlr3tuning::mlr3tuning.early_stopping",
    on_optimization_begin = function(callback, context) {
      learner = context$instance$objective$learner

      if (learner$id %nin% c("classif.xgboost", "regr.xgboost", "surv.xgboost")) {
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
            state$model$niter # GraphLearner state$model$xgboost$model$niter
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
#' This [Callback] writes the [BenchmarkResult] after each batch to disk.
#'
#' @examples
#' clbk("mlr3tuning.backup", path = "backup.rds")
NULL

load_callback_backup = function() {
  callback_tuning("mlr3tuning.backup",
    label = "Backup Benchmark Result Callback",
    man = "mlr3tuning::mlr3tuning.backup",
    on_optimization_begin = function(callback, context) {
      assert_path_for_output(callback$state$path)
    },

    on_optimizer_after_eval = function(callback, context) {
      if (file.exists(callback$state$path)) unlink(callback$state$path)
      saveRDS(context$instance$archive$benchmark_result, callback$state$path)
    },
    fields = list(path = "bmr.rds")
  )
}


