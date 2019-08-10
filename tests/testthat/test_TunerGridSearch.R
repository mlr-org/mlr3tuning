context("TunerGridSearch")


test_that("TunerGridSearch", {
  tuner = test_tuner(TunerGridSearch, list(resolution = 7), term_evals = 5L, real_evals = 5, n_dim = 1L)

  z = test_tuner(TunerGridSearch, list(resolution = 3), term_evals = 999L, real_evals = 9, n_dim = 2L)
  a = z$pe$archive()
  expect_data_table(a, nrows = 9)
  expect_set_equal(unique(a$cp), c(0.1, 0.2, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))

  z = test_tuner(TunerGridSearch, list(param_resolutions = c(cp = 2L, minsplit = 3L)), term_evals = 999L, real_evals = 6L, n_dim = 2L)
  a = z$pe$archive()
  expect_data_table(a, nrows = 6L)
  expect_set_equal(unique(a$cp), c(0.1, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))
})

test_that("TunerGridSearch with TerminatorNone", {
  pe = TEST_MAKE_PE1(n_dim = 2L)
  term = TerminatorNone$new()
  tuner = TunerGridSearch$new(resolution = 2L)
  r = tuner$tune(pe)
  bmr = pe$bmr
  expect_data_table(bmr$data, nrows = 8L)
})


