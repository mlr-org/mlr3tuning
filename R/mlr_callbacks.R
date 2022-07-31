#' @title Early Stopping Callback
#'
#' @description
#' Early Stopping Callback
#'
#' @name mlr3tuning.early_stopping
NULL

callback_early_stopping = as_callback("mlr3tuning.early_stopping",
  label = "Early Stopping Callback",
  on_optimization_begin = function(callback, context) {
    learner = context$instance$objective$learner

    if (learner$id %nin% c("classif.xgboost", "regr.xgboost", "surv.xgboost")) {
      stopf("%s is incompatible with %s", format(learner), format(callback))
    }

    # store models temporary
    callback$store_models = context$instance$objective$store_models
    context$instance$objective$store_models = TRUE

    if (is.null(learner$param_set$values$early_stopping_rounds)) {
      stop("Early stopping is not activated. Set `early_stopping_rounds` parameter.")
    }
  },

  on_eval_after_benchmark = function(callback, context) {
    callback$max_nrounds = map_dbl(context$benchmark_result$resample_results$resample_result, function(rr) {
        max(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) {
          state$model$niter # GraphLearner state$model$xgboost$model$niter
        }))
    })
  },

  on_eval_before_archive = function(callback, context) {
    set(context$aggregated_performance, j = "max_nrounds", value = callback$max_nrounds)
    if (!callback$store_models) context$benchmark_result$discard(models = TRUE)
  },

  on_result = function(callback, context) {
    context$result$learner_param_vals[[1]]$early_stopping_rounds = NULL
    context$result$learner_param_vals[[1]]$nrounds = context$instance$archive$best()$max_nrounds
    context$instance$objective$store_models = callback$store_models
  }
)

mlr_callbacks$add("early_stopping", callback_early_stopping)
