context("TunerGridSearch")


test_that("TunerGridSearch", {
  reso = 7L
  test_tuner(TunerGridSearch, list(resolution = reso))
})

