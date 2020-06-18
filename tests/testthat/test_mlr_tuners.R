context("mlr_optimizers")

test_that("mlr_tuners", {
  expect_dictionary(mlr_optimizers)
  for (key in mlr_optimizers$keys()) {
    expect_optimizer(opt(key))
  }
})
