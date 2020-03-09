context("TunerIrace")

skip_if_not_installed("irace")

test_that("TunerIrace", {
  test_tuner("irace", term_evals = 38)
})

test_that("TunerIrace with int params and trafo, clock terminator", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$trafo = function(x, param_set) {
    x$minsplit = as.integer(round(x$minsplit))
    return(x)
  }
  te = term("clock_time", secs = 5)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = tnr("irace")
  tt$tune(inst)
  d = inst$archive(unnest = "params")
  expect_integer(d$minsplit)
})

test_that("minimize time",{
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  te = term("clock_time", secs = 5)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = tnr("irace", capping = 1, boundMax = 1, cappingType = "best", boundType= "instance")
  tt$tune(inst)
  expect_double(inst$archive(unnest = "params")$minsplit)
})
