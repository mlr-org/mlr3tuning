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

get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}

#' @title Default optimization function
#' @description
#' Used internally in the [Optimizer].
#' Brings together the private `.optimize()` method and the private `.assign_result()` method.
#'
#' @param inst [OptimInstance]
#' @param self [Optimizer]
#' @param private (`environment()`)
#'
#' @return [data.table::data.table]
#'
#' @keywords internal
#' @export
optimize_tuning = function(inst, self, private) {
  assert_instance_properties(self, inst)
  inst$archive$start_time = Sys.time()
  if (isNamespaceLoaded("progressr")) {
    # initialize progressor
    # progressor must be initialized here because progressor finishes when exiting a function since version 0.7.0
    max_steps = assert_int(inst$terminator$status(inst$archive)["max_steps"])
    unit = assert_character(inst$terminator$unit)
    progressor = progressr::progressor(steps = max_steps)
    inst$progressor = Progressor$new(progressor, unit)
    inst$progressor$max_steps = max_steps
  }

  # start optimization
  lg$info("Starting to optimize %i parameter(s) with '%s' and '%s'",
    inst$search_space$length, self$format(), inst$terminator$format(with_params = TRUE))
  tryCatch({
    private$.optimize(inst)
  }, terminated_error = function(cond) {
  })

  # if evaluation was asynchronous, combine resample results to single benchmark result
  if (!is.null(inst$archive$data$resample_result)) {
    rdatas = map(unlist(inst$archive$data$resample_result), function(rr) get_private(rr)$.data)
    if (length(rdatas) > 1) for (i in 2:length(rdatas)) rdatas[[1]]$combine(rdatas[[i]])
    inst$archive$benchmark_result = BenchmarkResult$new(rdatas[[1]])
    inst$archive$data["evaluated", uhash := inst$archive$benchmark_result$uhashes, on = "status"]
    inst$archive$data[, resample_result := NULL]
  }

  private$.assign_result(inst)
  lg$info("Finished optimizing after %i evaluation(s)", inst$archive$n_evals)
  lg$info("Result:")
  lg$info(capture.output(print(
    inst$result, lass = FALSE, row.names = FALSE, print.keys = FALSE)))
  return(inst$result)
}
