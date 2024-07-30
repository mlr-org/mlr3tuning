measures_to_codomain = function(measures) {
  measures = as_measures(measures)
  domains = map(measures, function(s) {
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
  internal_search_space$aggr_internal_tuned_values(internal_tuned_values)
}

# init_internal_search_space = function(self, private, super, search_space, store_benchmark_result, learner, callbacks, batch) {
#   assert_flag(store_benchmark_result)
#   internal_search_space = NULL
#   internal_tune_ids = keep(names(search_space$tags), map_lgl(search_space$tags, function(t) "internal_tuning" %in% t))

#   if (length(internal_tune_ids)) {
#     internal_search_space = search_space$subset(internal_tune_ids)
#     if (internal_search_space$has_trafo) {
#       stopf("Inner Tuning and Parameter Transformations are currently not supported.")
#     }
#     search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))

#     # the learner dictates how to interprete the to_tune(..., inner)

#     learner$param_set$set_values(
#       .values = learner$param_set$convert_internal_search_space(internal_search_space)
#     )

#     # we need to use a callback to change how the Optimizer writes the result to the ArchiveTuning
#     # This is because overwriting the Tuner's .assign_result method has no effect, as it is not called.helper
#     callbacks = c(load_callback_internal_tuning(batch), callbacks)
#   }

#   list(
#     search_space = search_space,
#     callbacks = callbacks,
#     internal_search_space = internal_search_space %??% ps()
#   )
# }

# init_internal_search_space_archive = function(self, private, super, search_space, internal_search_space) {
#   if (!is.null(internal_search_space)) {
#     private$.internal_search_space = as_search_space(internal_search_space)
#     assert_disjunct(search_space$ids(), internal_search_space$ids())
#   } else {
#     private$.internal_search_space = ps()
#   }
# }
