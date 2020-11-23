test_that("TunerGridSearch", {
  test_tuner("grid_search", resolution = 7, term_evals = 5L, real_evals = 5, n_dim = 1L)
  test_tuner_dependencies("grid_search")

  z = test_tuner("grid_search", resolution = 3, term_evals = 999L, real_evals = 9, n_dim = 2L)
  a = z$inst$archive$data()
  expect_data_table(a, nrows = 9)
  expect_set_equal(unique(a$cp), c(0.1, 0.2, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))

  z = test_tuner("grid_search", param_resolutions = c(cp = 2L, minsplit = 3L), term_evals = 999L, real_evals = 6L, n_dim = 2L)
  a = z$inst$archive$data()
  expect_data_table(a, nrows = 6L)
  expect_set_equal(unique(a$cp), c(0.1, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))
})

test_that("TunerGridSearch with TerminatorNone", {
  inst = TEST_MAKE_INST1(n_dim = 2L)
  term = TerminatorNone$new()
  tuner = tnr("grid_search", resolution = 2L)
  r = tuner$optimize(inst)
  archive = inst$archive
  expect_data_table(archive$data(), nrows = 4L)
})
