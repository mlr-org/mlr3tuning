test_that("simple exp trafo works", {
  ll = lrn("classif.rpart")
  ps = ps(
    cp = p_dbl(lower = -8, upper = -2, trafo = function(x) 2^x)
  )
  te = trm("evals", n_evals = 3)
  d = data.table(cp = c(-7, -3))
  tuner = tnr("design_points", design = d)
  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), ll, rsmp("holdout"), msr("dummy.cp.classif", fun = function(pv) pv$cp), te, ps)
  tuner$optimize(inst)
  expect_equal(inst$result_x_search_space, data.table(cp = -7))
  expect_equal(inst$result_learner_param_vals, list(xval = 0, cp = 2^-7))
  expect_equal(inst$result_y, c(dummy.cp.classif = 2^-7))
  a = inst$archive$data
  expect_equal(a$x_domain, list(list(cp = 2^-7), list(cp = 2^-3)))
})

test_that("trafo where param names change", {
  ll = lrn("classif.rpart")
  ps = ps(
    foo = p_fct(levels = c("a", "b")),
    .extra_trafo = function(x, param_set) {
      if (x$foo == "a")
        x$cp = 0.11
      else
        x$cp = 0.22
      x$foo = NULL
      return(x)
    }
  )
  te = trm("evals", n_evals = 3)
  tuner = tnr("grid_search", resolution = 2)
  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), ll, rsmp("holdout"), msr("dummy.cp.classif", fun = function(pv) pv$cp), te, ps)
  tuner$optimize(inst)
  expect_equal(inst$result_x_search_space, data.table(foo = "a"))
  expect_equal(inst$result_learner_param_vals, list(xval = 0, cp = 0.11))
  expect_equal(inst$result_y, c(dummy.cp.classif = 0.11))
  a = inst$archive$data
  expect_setequal(unlist(a$x_domain), c(0.11, 0.22)) # expect_equal not working since TunerGridSearch shuffles points
})

