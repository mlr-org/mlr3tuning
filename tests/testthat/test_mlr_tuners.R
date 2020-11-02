test_that("mlr_tuners", {
  expect_dictionary(mlr_tuners)
  for (key in mlr_tuners$keys()) {
    expect_tuner(tnr(key))
  }
})
