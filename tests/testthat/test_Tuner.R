context("Tuner")


test_that("API", {
  measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))
  for (n_evals in c(1, 5)) {
    rs = TunerRandomSearch$new()
    pe = TEST_MAKE_INST1(measures = measures, term_evals = n_evals)
    r = rs$tune(pe)
    a = pe$archive()
    expect_data_table(a, nrows = n_evals)
    expect_true("cp" %in% names(a))
    expect_true("params" %in% names(pe$archive(FALSE)))
  }
})
