context("TunerNloptr")

skip_if_not_installed("nloptr")

test_that("TunerNloptr", {
  test_tuner("nloptr")

  ps = ParamSet$new(params = list(
    ParamLgl$new("save_tasks")
  ))
  te = term("evals", n_evals = 2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = TunerNloptr$new()
  expect_error(tt$tune(inst), "support")
})

test_that("TunerNloptr with int params and trafo", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$trafo = function(x, param_set) {
    x$minsplit = as.integer(round(x$minsplit))
    return(x)
  }
  te = term("evals", n_evals = 2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = TunerNloptr$new()
  tt$tune(inst)
  d = inst$archive(unnest = "params")
  expect_integer(d$minsplit)
})
