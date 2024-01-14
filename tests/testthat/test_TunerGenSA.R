skip_if_not_installed("GenSA")

test_that("TunerGenSA", {
  test_tuner("gensa")

  ps = ps(
    save_tasks = p_lgl()
  )
  te = trm("evals", n_evals = 2)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), msr("classif.ce"), te, ps)
  tt = TunerGenSA$new()
  expect_error(tt$optimize(inst), "support")
})

test_that("TunerGenSA with int params and trafo", {
  ps = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_dbl(lower = 1, upper = 10, trafo = function(x) as.integer(round(x)))
  )
  te = trm("evals", n_evals = 2)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), te, ps)
  tt = TunerGenSA$new()
  tt$optimize(inst)
  d = inst$archive$data
  expect_integer(d$x_domain[[1]]$minsplit)
})

test_that("TunerGenSA - Optimize wrapper with maximize measure", {
  inst = TEST_MAKE_INST1(measure = msr("dummy.cp.maximize.classif", function(pv)  pv$cp), n_dim = 1)
  tt = tnr("gensa", smooth = TRUE)
  tt$optimize(inst)

  res = inst$archive$best()
  expect_equal(res$cp, max(inst$archive$data$cp))
})
