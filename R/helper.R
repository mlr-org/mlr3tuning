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

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}

