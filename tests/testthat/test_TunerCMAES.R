context("TunerCMAES")

skip_if_not_installed("cmaes")

test_that("TunerCMAES", {
  test_tuner("cmaes", real_evals = 4)
})

test_that("TunerCMAES - Optimize wrapper with maximize measure", {
  inst = TEST_MAKE_INST1(measures = msr("dummy.cp.maximize.classif"), n_dim = 1)
  tt = TunerCMAES$new()
  tt$optimize(inst)

  res = inst$archive$get_best()
  expect_equal(res$cp, max(inst$archive$data$cp))
})

