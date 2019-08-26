context("mlr_terminators")

test_that("mlr_terminators", {
  expect_dictionary(mlr_terminators)
  # for (key in mlr_terminators$keys()) {
  #   term = mlr_terminators$get(key)
  #   expect_terminator(term)
  # }
})

test_that("mlr_terminators sugar", {
  term = term("evals", n_evals = 10)
  expect_terminator(term)
})
