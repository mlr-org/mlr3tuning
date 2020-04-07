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

# get an object from a list of id-able-objects, like many from mlr3
# returns NULL if not found
get_by_id = function(xs, id) {
  for (x in xs) {
    if (x$id == id) {
      return(x)
    }
  }
  return(NULL)
}
