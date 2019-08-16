context("TunerGenSA")


test_that("TunerGenSA", {
  test_tuner(TunerGenSA)

  ps = ParamSet$new(params = list(
    ParamLgl$new("save_tasks")
  ))
  term = TerminatorEvals$new(2)
  pe = TuningInstance$new("iris", "classif.debug", "holdout", "classif.ce", ps, term)
  tt = TunerGenSA$new()
  expect_error(tt$tune(pe), "support")
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
  pe = TuningInstance$new("iris", "classif.rpart", "holdout", "classif.ce", ps, term)
  tt = TunerGenSA$new()
  tt$tune(pe)
  d = pe$archive()
  expect_integer(d$minsplit)
})
