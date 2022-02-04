terminated_error = function(instance) {
  msg = sprintf(
    fmt = "TuningInstance (tsk:%s, lrn:%s, term:%s) terminated",
    instance$objective$task$id,
    instance$objective$learner$id,
    format(instance$terminator)
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

evaluate_default = function(self) {
  # get hyperparameter defaults
  # values are on the learner scale i.e. possible transformation are already applied
  xss = default_values(self$objective$learner, self$search_space, self$objective$task)

  # parameters with exp transformation and log inverse transformation
  has_logscale = map_lgl(self$search_space$params, function(param) get_private(param)$.has_logscale)
  # parameters with unknown inverse transformation
  has_trafo = map_lgl(self$search_space$params, function(param) get_private(param)$.has_trafo)

  # get values on tuner scale
  xdt = if (get_private(self$search_space)$.has_extra_trafo) {
    # parameter set has a trafo which could affect every parameter
    # inverse transformation for all parameters is unknown
    as.data.table(xss)[0, ]
  } else {
    # apply inverse transformation and keep parameter values without transformation function unchanged
    defaults = map_if(xss, has_logscale, log)
    # set parameters without inverse transformation to NA
    defaults[has_trafo] = NA
    as.data.table(defaults)
  }

  # evaluate default values
  ydt = self$objective$eval_many(list(xss))
  # temporary disable check values to allow NA
  old_check_values = self$archive$check_values
  self$archive$check_values = FALSE
  # add to archie
  self$archive$add_evals(xdt, list(xss), ydt)
  self$archive$check_values = old_check_values

  lg$info("Result of default configuration:")
  lg$info(capture.output(print(cbind(xdt, ydt),
    class = FALSE, row.names = FALSE, print.keys = FALSE)))
}
