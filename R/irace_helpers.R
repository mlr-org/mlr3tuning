paradox_to_irace = function(ps) {

  assertClass(ps, "ParamSet")
  # what about ParamUty = vector numeric/real
  class_lookup = data.table(
    paradox = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
    irace = c("c", "i", "r", "c"), stringsAsFactors = FALSE)

  type = unlist(subset(merge(data.table(paradox = ps$class), class_lookup, sort = FALSE),
    select = "irace"))
  range = get_irace_range(ps)
  if (ps$has_deps) {
    condition = get_irace_condition(ps)
  } else {
    condition = NULL
  }

  par_tab = paste(ps$ids(), '""', type, range, condition$cond, collapse = "\n")

  return(irace::readParameters(text = par_tab))
}
get_irace_range = function(ps) {
  rng = data.table(lower = ps$lower, upper = ps$upper, lvl = ps$levels)

  apply(rng, 1, function(x) {
    if (is.na(x[[1]])) {
      return(paste0("(", paste0(x[[3]], collapse = ","), ")"))
    } else {
      return(paste0("(", x[[1]], ",", x[[2]], ")"))
    }
  })
}
get_irace_condition = function(ps) {
  cond = rbindlist(apply(ps$deps, 1, function(x) {
    on = x[[2]]
    cond = x[[3]]$rhs
    if (is.character(cond)) {
      cond = paste0("'", cond, "'")
    }
    if (x[[3]]$type == "equal") {
      condition = paste("|", x[[2]], "==", cond)
    } else {
      condition = paste("|", x[[2]], "%in%", paste0("c(", paste0(cond, collapse = ","), ")"))
    }
    data.table(id = x[[1]], cond = condition)
  }))

  # coercion back and forth from frame/table is due to data.frame sorting even when sort = FALSE
  tab = data.frame(merge(data.table(id = ps$ids()), cond, by = "id", all.x = TRUE, sort = FALSE))
  tab[is.na(tab)] = ""

  return(tab)
}


targetRunner = function(experiment, scenario) { # nolint
  t0 = Sys.time()
  tuning_instance = scenario$targetRunnerData$inst

  # fix logicals
  config = as.data.table(lapply(experiment$configuration, function(x) {
    if (x %in% c("TRUE", "FALSE")) {
      return(as.logical(x))
    } else {
      return(x)
    }
  }))

  # change resampling instance
  tuning_instance$objective$resampling = experiment$instance

  # add extra info to archive
  extra = data.table(id_configuration = experiment$id.configuration, id_instance = experiment$id.instance)

  # evaluate configuration
  # objective_function cannot pass extra information
  cost = as.numeric(tuning_instance$eval_batch(cbind(config, extra))) * tuning_instance$objective_multiplicator
  t1 = Sys.time()

  return(list(cost = cost, time = as.numeric(t1 - t0)))
}
