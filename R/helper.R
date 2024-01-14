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
