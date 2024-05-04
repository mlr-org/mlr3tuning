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

evaluate_default = function(inst) {
  # values are on the learner scale i.e. possible transformation are already applied
  xss = default_values(inst$objective$learner, inst$search_space, inst$objective$task)

  # parameters with exp transformation and log inverse transformation
  # parameters with unknown inverse transformation
  # parameter set with trafo
  if ("set_id" %in% names(ps())) {
    # old paradox
    has_logscale = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_logscale)

    has_trafo = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_trafo)

    has_extra_trafo = get_private(inst$search_space)$.has_extra_trafo
  } else {
    has_logscale = map_lgl(inst$search_space$params$.trafo, function(x) identical(x, exp))

    has_trafo = map_lgl(inst$search_space$params$.trafo, function(x) !is.null(x) && !identical(x, exp))

    has_extra_trafo = !is.null(inst$search_space$extra_trafo)
  }

  if (any(has_trafo) || has_extra_trafo) {
    stop("Cannot evaluate default hyperparameter values. Search space contains transformation functions with unknown inverse function.")
  }

  # inverse parameter with exp transformation
  xdt = as.data.table(map_if(xss, has_logscale, log))

  inst$eval_batch(xdt)
}

# this function translates things like nrounds = p_int(upper = 1000, inner = TRUE) to the actual value used by the learner,
# in this case, the upper value 1000. The information on how to translate is stored in the translator function.
# The param_set is the parameter set of the learner, possbily containing other information for the translation.
#
# search_space: The search space that con
convert_inner_tune_tokens = function(search_space, param_set) {
  imap(search_space$domains, function(domain, .__id) {
    param_set$params[.__id, "cargo", on = "id"][[1L]][[1L]]$tune_converter(domain, param_set)
  })
}
