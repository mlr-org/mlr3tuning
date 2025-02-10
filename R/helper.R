measures_to_codomain = function(measures) {
  measures = as_measures(measures)
  domains = map(measures, function(s) {
    if (is.na(s$minimize)) {
      stopf("Measure %s has its `minimize` field set to NA, which is disallowed when tuning.", s$id)
    }
    p_dbl(tags = ifelse(s$minimize, "minimize", "maximize"))
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

extract_inner_tuned_values = function(resample_result, internal_search_space) {
  internal_tuned_values = transpose_list(map(get_private(resample_result)$.data$learner_states(get_private(resample_result)$.view), "internal_tuned_values"))
  set_class(internal_search_space$aggr_internal_tuned_values(internal_tuned_values), "internal_tuned_values")
}


split_internal_search_space = function(search_space) {
  internal_tune_ids = search_space$ids(any_tags = "internal_tuning")
  if (length(internal_tune_ids)) {
    internal_search_space = search_space$subset(internal_tune_ids)
    search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))
    return(list(search_space = search_space, internal_search_space = internal_search_space))
  }
  list(search_space = search_space, internal_search_space = NULL)
}

#' @export
format_list_item.internal_tuned_values = function(x, ...) {
  as_short_string(unclass(x))
}
