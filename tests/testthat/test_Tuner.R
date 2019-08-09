context("Tuner")


test_that("API", {
  measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))
  for (n_evals in c(1,5)) {
    rs = TunerRandomSearch$new()
    pe = TEST_MAKE_PE1(measures = measures, term_evals = n_evals)
    rs$pe = pe
    rs$tune()
    expect_data_table(rs$archive(), nrows = n_evals)
    expect_true("cp" %in% names(rs$archive()))
    expect_true("params" %in% names(rs$archive(FALSE)))
  }
})

