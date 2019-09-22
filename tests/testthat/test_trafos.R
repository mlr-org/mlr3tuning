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
  r = inst$result
  expect_equal(r$tune_x, list(cp = -7))
  expect_equal(r$params, list(xval = 0, cp = 2^-7))
  expect_equal(r$perf, c(dummy.cp.classif = 2^-7))
  a = inst$archive(unnest = "params")
  expect_equal(a$cp, c(2^-7, 2^-3))
})

test_that("trafo where param names change", {
  ll = lrn("classif.rpart")
  ps = ParamSet$new(params = list(
    ParamFct$new("foo", levels = c("a", "b"))
  ))
  ps$trafo = function(x, param_set) {
    if (x$foo == "a")
      x$cp = 0.11
    else
      x$cp = 0.22
    x$foo = NULL
    return(x)
  }
  te = term("evals", n_evals = 3)
  tuner = tnr("grid_search", resolution = 2)
  inst = TuningInstance$new(tsk("iris"), ll, rsmp("holdout"), msr("dummy.cp.classif"), ps, te)
  tuner$tune(inst)
  r = inst$result
  expect_equal(r$tune_x, list(foo = "a"))
  expect_equal(r$params, list(xval = 0, cp = 0.11))
  expect_equal(r$perf, c(dummy.cp.classif = 0.11))
  a = inst$archive(unnest = "params")
  expect_set_equal(a$cp, c(0.11, 0.22)) # NB: grid search randomly permutes order
})

