context("trafos")

test_that("simple exp trafo works", {
  ll = lrn("classif.rpart")
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = -8, upper = -2)
  ))
  ps$trafo = function(x, param_set) {
    x$cp = 2^x$cp
    return(x)
  }
  te = term("evals", n_evals = 3)
  d = data.table(cp = c(-7, -3))
  tuner = tnr("design_points", design = d)
  inst = TuningInstance$new(tsk("iris"), ll, rsmp("holdout"), msr("dummy.cp.classif"), ps, te)
  tuner$tune(inst)
  r = inst$result(complete = FALSE)
  expect_equal(r$config, list(cp = -7))
  expect_equal(r$config_trafo, list(cp = 2^-7))
  expect_equal(r$perf, c(dummy.cp.classif = 2^-7))
  a = inst$archive()
  expect_equal(a$cp, c(2^-7, 2^-3))
})
