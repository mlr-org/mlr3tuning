context("TunerGridSearch")


test_that("TunerGridSearch", {
  tuner = test_tuner(TunerGridSearch, list(resolution = 7), term_evals = 5L, real_evals = 5, n_dim = 1)

  tuner = test_tuner(TunerGridSearch, list(resolution = 3), term_evals = 999L, real_evals = 9, n_dim = 2)
  expect_equal(tuner$pe$n_evals, 9)
  a = tuner$aggregate()
  expect_data_table(a, nrows = 9)
  expect_set_equal(unique(a$cp), c(0.1, 0.2, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))
})

