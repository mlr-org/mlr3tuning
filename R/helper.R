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

hash = function(...) {
  dots = list(...)
  dots = map_if(dots, is.function, function(fun) {
    list(formals(fun), as.character(body(fun)))
  })
  dots = map_if(dots, is.data.table, as.list)
  digest::digest(dots, algo = "xxhash64")
}
