context("OptimizerRandomSearch")

test_that("OptimizerRandomSearch", {
  test_tuner("random_search")
  test_tuner_dependencies("random_search")
})
