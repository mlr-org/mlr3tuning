terminated_error = function(inst) {
  msg = sprintf(
    fmt = "TuningInstance (tsk:%s, lrn:%s, term:%s) terminated",
    inst$objective$task$id,
    inst$objective$learner$id,
    format(inst$terminator)
  )

  set_class(list(message = msg, call = NULL), c(
    "terminated_error", "error", "condition"))
}

measures_to_codomain = function(measures) {
  Codomain$new(map(as_measures(measures), function(s) {
    ParamDbl$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
  }))
}

extract_benchmark_result_learners = function(bmr) {
  unlist(map(seq_len(bmr$n_resample_results), function(n) {
    bmr$resample_result(n)$learners
  }))
}

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}

evaluate_default = function(inst) {
  # values are on the learner scale i.e. possible transformation are already applied
  xss = default_values(inst$objective$learner, inst$search_space, inst$objective$task)

  # parameters with exp transformation and log inverse transformation
  has_logscale = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_logscale)
  # parameters with unknown inverse transformation
  has_trafo = map_lgl(inst$search_space$params, function(param) get_private(param)$.has_trafo)
  # parameter set with trafo
  has_extra_trafo = get_private(inst$search_space)$.has_extra_trafo

  if (any(has_trafo) || has_extra_trafo) {
    stop("Cannot evaluate default hyperparameter values. Search space contains transformation functions with unknown inverse function.")
  }

  # inverse parameter with exp transformation
  xdt = as.data.table(map_if(xss, has_logscale, log))

  inst$eval_batch(xdt)
}
