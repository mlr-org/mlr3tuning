paradox_to_irace = function(ps){
  assertClass(ps, "ParamSet")
  # what about ParamUty = vector numeric/real
  class_lookup = data.table(paradox = c("ParamLgl","ParamInt","ParamDbl","ParamFct"),
                            irace = c("c","i","r","c"), stringsAsFactors = FALSE)

  type = unlist(subset(merge(data.table(paradox = ps$class), class_lookup, sort = FALSE), select = "irace"))
  range = get_irace_range(ps)
  if (ps$has_deps) {
    condition = get_irace_condition(ps)
  } else {
    condition = NULL
  }

  par.tab = paste(ps$ids(), '""', type, range, condition$cond, collapse = "\n")

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
    on = x[[2]]
    cond = x[[3]]$rhs
    if (is.character(cond)) {
      cond = paste0("'", cond ,"'")
    }
    if (x[[3]]$type == "equal") {
      condition = paste("|",x[[2]], "==", cond)
    } else {
      condition = paste("|",x[[2]],"%in%", paste0("c(",paste0(cond, collapse = ","),")"))
    }
    data.table(id = x[[1]], cond = condition)
  }))

  # coercion back and forth from frame/table is due to data.frame sorting even when sort = FALSE
  tab = data.frame(merge(data.table(id = ps$ids()), cond, by = "id", all.x = TRUE, sort = FALSE))
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
  config = as.data.table(lapply(experiment$configuration, function(x){
    if (x %in% c("TRUE", "FALSE")) {
      return(as.logical(x))
    } else {
      return(x)
    }
  }))
  cost = scenario$instances[[1]]$eval_batch(config)$perf
  t1 = Sys.time()

  return(list(cost = cost, time = as.numeric(t1-t0)))
}
