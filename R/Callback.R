# max_nrounds_callback
max_nrounds_callback = mlr3_callback("max_nrounds_callback",
  on_eval_after_benchmark = function(context) {
    mean_nrounds = round(map_dbl(context$benchmark_result$resample_results$resample_result, function(rr) {
        mean(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) {
          if (is.null(state$model$xgboost$model)) NA else state$model$xgboost$model$niter
        }))
    }))
    set(context$aggregated_performance, j = "mean_nrounds", value = mean_nrounds)
  },

  on_result = function(context) {
    context$get_private(instance)$.result
  }
)

# backup_callback
backup_callback = mlr3_callback("backup_callback",
  x  = function(context) {

  }
)


context = Context$new()
max_nrounds_callback$call("on_eval_after_design", context)
