measures_to_codomain = function(measures) {
  measures = as_measures(measures)
  domains = map(measures, function(s) {
    if ("set_id" %in% names(ps())) {
      # old paradox
      get("ParamDbl")$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
    } else {
      p_dbl(tags = ifelse(s$minimize, "minimize", "maximize"))
    }
  })
  names(domains) = ids(measures)
  Codomain$new(domains)
}

extract_benchmark_result_learners = function(bmr) {
  unlist(map(seq_len(bmr$n_resample_results), function(n) {
    bmr$resample_result(n)$learners
  }))
}

extract_runtime = function(resample_result) {
  runtimes = map_dbl(get_private(resample_result)$.data$learner_states(get_private(resample_result)$.view), function(state) {
    state$train_time + state$predict_time
  })
  sum(runtimes)
}
