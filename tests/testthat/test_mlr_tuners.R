context("mlr_tuners")

test_that("mlr_tuners", {
  expect_dictionary(mlr_tuners)
  for (key in mlr_tuners$keys()) {
    tuner = mlr_tuners$get(key)
    expect_tuner(tuner)
  }
})

test_that("mlr_tuners sugar", {
  tuner = tnr("random_search")
  expect_tuner(tuner)
})
