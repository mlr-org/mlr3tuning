test_that("TunerDesignPoints", {
  d = data.table(cp = c(0.1, 0.3))
  test_tuner("design_points", design = d, term_evals = 2L, real_evals = 2, n_dim = 1L)

  d = data.table(xx = c("a", "b"), yy = c(1, NA), cp = c(0.1, 0.2))
  test_tuner_dependencies("design_points", design = d)

  d = data.table(cp = c(0.1, 0.3), minsplit = c(1, 2))
  z = test_tuner("design_points", design = d, term_evals = 999L, real_evals = 2L, n_dim = 2L)
  a = z$inst$archive$data()
  expect_data_table(a, nrows = 2)
  expect_set_equal(a$cp, c(0.1, 0.3))
  expect_set_equal(a$minsplit, c(1, 2))

  expect_error(test_tuner("design_points", term_evals = 2L, real_evals = 2, n_dim = 1L))
})

