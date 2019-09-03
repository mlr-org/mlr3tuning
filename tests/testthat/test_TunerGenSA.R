context("TunerGenSA")


test_that("TunerGenSA", {
  test_tuner("gensa")

  ps = ParamSet$new(params = list(
    ParamLgl$new("save_tasks")
  ))
  term = TerminatorEvals$new(2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), msr("classif.ce"), ps, term)
  tt = TunerGenSA$new()
  expect_error(tt$tune(inst), "support")
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
  term = TerminatorEvals$new(2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, term)
  tt = TunerGenSA$new()
  tt$tune(inst)
  d = inst$archive()
  expect_integer(d$minsplit)
})
