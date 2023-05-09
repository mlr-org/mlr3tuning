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

#' @export
workhorse = function(task, learner, resampling, measure, store_benchmark_result, store_models, callbacks, config) {
  r = redux::hiredis(config)
  while (r$GET("terminate") == "0") {
    bin_xss = r$RPOP("queue")
    if (!is.null(bin_xss)) {
      xss = redux::bin_to_object(bin_xss)
      learner = learner$clone(deep = TRUE)
      learner$param_set$set_values(.values = xss)
      rr = resample(task, learner, resampling, store_models = store_models, clone = character())
      ydt = as.data.table(as.list(rr$aggregate()))
      runtime_learners = sum(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) state$train_time + state$predict_time))
      set(ydt , j = "runtime_learners", value = runtime_learners)
      if (store_benchmark_result) set(ydt , j = "resample_result", value = list(rr))
      r$LPUSH("result", redux::object_to_bin(ydt))
    } else {
      Sys.sleep(0.1)
    }
  }
}

start_workers = function(inst) {
  n = future::nbrOfWorkers()
  r = redux::hiredis(inst$objective$redis_config)
  r$FLUSHDB()
  r$SET("terminate", FALSE)

  inst$objective$promises = replicate(n, future::future(mlr3tuning::workhorse(
    task = inst$objective$task,
    learner = inst$objective$learner,
    resampling = inst$objective$resampling,
    measure = inst$objective$measure,
    store_benchmark_result = inst$objective$store_benchmark_result,
    store_models = inst$objective$store_models,
    callbacks = inst$objective$callbacks,
    config = inst$objective$redis_config)))
}

kill_workers = function(inst) {
  r = redux::hiredis(inst$objective$redis_config)
  r$SET("terminate", TRUE)
}
