context("Tuner")


test_that("API", {
  measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))
  pe = TEST_MAKE_PE1(measures = measures)
  for (n_evals in c(1,5)) {
    terminator = TerminatorEvaluations$new(n_evals)
    rs = TunerRandomSearch$new(pe$clone(), terminator)
    expect_error(rs$archive())
    rs$tune()
    expect_data_table(rs$archive(), nrows = n_evals)
    expect_true("cp" %in% names(rs$archive()))
    expect_true("params" %in% names(rs$archive(FALSE)))
  }
})

