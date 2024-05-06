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

# this function translates things like nrounds = p_int(upper = 1000, inner = TRUE) to the actual value used by the learner,
# in this case, the upper value 1000. The information on how to translate is stored in the translator function.
# The param_set is the parameter set of the learner, possbily containing other information for the translation.
#
# search_space: The search space that con
convert_inner_tune_tokens = function(search_space, param_set) {
  imap(search_space$domains, function(domain, .__id) {
    param_set$params[.__id, "cargo", on = "id"][[1L]][[1L]]$in_tune_fn(domain, param_set)
  })
}
