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
