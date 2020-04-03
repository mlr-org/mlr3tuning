context("TunerGenSA")

skip_if_not_installed("GenSA")

test_that("TunerGenSA", {
  test_tuner("gensa")

  ps = ParamSet$new(params = list(
    ParamLgl$new("save_tasks")
  ))
  te = term("evals", n_evals = 2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = TunerGenSA$new()
  expect_error(tt$optimize(inst), "support")
})

test_that("TunerGenSA with int params and trafo", {
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
  tt = TunerGenSA$new()
  tt$optimize(inst)
  d = inst$archive$data
  expect_integer(d$opt_x[[1]]$minsplit)
})
