context("TunerRandomSearch")

test_that("TunerRandomSearch", {
  test_tuner(TunerRandomSearch)
  test_tuner_dependencies(TunerRandomSearch)
})
