paradoxToIrace = function(ps){
  assertClass(ps, "ParamSet")
  # what about ParamUty = vector numeric/real
  class_lookup = data.frame(paradox = c("ParamLgl","ParamInt","ParamDbl","ParamFct"),
                            irace = c("c","i","r","c"), stringsAsFactors = FALSE)
  name <- ps$ids()
  type <- class_lookup[match(ps$class, class_lookup$paradox), 2]
  range <- getIraceRange(ps)
  if(ps$has_deps){
    condition <- getIraceCondition(ps)
  } else {
    condition = NULL
  }

  # does mlr3tuning deal with trafo internally or passed to irace? irace only handle log
  par.tab <- paste(name, '""', type, range, condition, collapse = "\n")
  irace::readParameters(text = par.tab)
}
getIraceRange = function(ps){
  rng = data.table(lower = ps$lower, upper = ps$upper, lvl = ps$levels)

  apply(rng, 1, function(x){
    if(is.na(x[[1]]))
      return(paste0("(",paste0(x[[3]], collapse = ","),")"))
    else
      return(paste0("(",x[[1]],",",x[[2]],")"))
  })
}
getIraceCondition = function(ps){
  cond = apply(ps$deps, 1, function(x){
    if(class(x[[3]])[1] == "CondEqual"){
      condition = paste("|",x[[2]], "==", x[[3]]$rhs)
    } else {
      condition = paste("|",x[[2]],"%in%", paste0("c(",paste0(x[[3]]$rhs, collapse = ","),")"))
    }
  })

  condition = data.table(id = ps$ids(), cond = "")
  ind = match(ps$deps$id, condition$id)
  condition$cond[ind] = cond
  condition$cond
}

makeScenario = function(instance){
  list(
    targetRunner = targetRunner,
    logFile = tempfile(),
    instances = list(instance),
    debugLevel = 0,
    maxExperiments = ifelse(class(instance$terminator)[1] == "TerminatorEvals",
                                       instance$terminator$param_set$values$n_evals,
                                       0),
    maxTime = ifelse(class(instance$terminator)[1] == "TerminatorClockTime",
                     instance$terminator$param_set$values$secs,
                     0)
  )
}

targetRunner = function(experiment, scenario){
  t0 = Sys.time()
  cost = scenario$instances[[1]]$eval_batch(as.data.table(experiment$configuration))$perf
  t1 = Sys.time()
  list(cost = cost, time = as.numeric(t1-t0))
}
