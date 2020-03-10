paradox_to_irace = function(ps){
  assertClass(ps, "ParamSet")
  # what about ParamUty = vector numeric/real
  class_lookup = data.table(paradox = c("ParamLgl","ParamInt","ParamDbl","ParamFct"),
                            irace = c("c","i","r","c"), stringsAsFactors = FALSE)

  type = unlist(subset(merge(class_lookup, data.table(paradox = ps$class)), select = "irace"))
  range = get_irace_range(ps)
  if (ps$has_deps) {
    condition = get_irace_condition(ps)
  } else {
    condition = NULL
  }

  par.tab = paste(ps$ids(), '""', type, range, condition, collapse = "\n")

  return(irace::readParameters(text = par.tab))
}
get_irace_range = function(ps){
  rng = data.table(lower = ps$lower, upper = ps$upper, lvl = ps$levels)

  apply(rng, 1, function(x){
    if (is.na(x[[1]])) {
      return(paste0("(",paste0(x[[3]], collapse = ","),")"))
    } else {
      return(paste0("(",x[[1]],",",x[[2]],")"))
    }
  })
}
get_irace_condition = function(ps){
  cond = rbindlist(apply(ps$deps, 1, function(x){
    if (x[[3]]$type == "equal") {
      condition = paste("|",x[[2]], "==", x[[3]]$rhs)
    } else {
      condition = paste("|",x[[2]],"%in%", paste0("c(",paste0(x[[3]]$rhs, collapse = ","),")"))
    }
    data.table(id = x[[1]], cond = condition)
  }))

  tab = merge(data.frame(id = ps$ids()), cond, by = "id", all.x = TRUE)
  tab[is.na(tab)] = ""

  return(tab)
}
make_scenario = function(instance){
  list(
    targetRunner = targetRunner,
    logFile = tempfile(),
    instances = list(instance),
    debugLevel = 0,
    maxExperiments = if (class(instance$terminator)[1] == "TerminatorEvals") instance$terminator$param_set$values$n_evals else 0,
    maxTime = if (class(instance$terminator)[1] == "TerminatorClockTime") instance$terminator$param_set$values$secs else 0
  )
}

targetRunner = function(experiment, scenario){
  t0 = Sys.time()
  cost = scenario$instances[[1]]$eval_batch(as.data.table(experiment$configuration))$perf
  t1 = Sys.time()

  return(list(cost = cost, time = as.numeric(t1-t0)))
}
