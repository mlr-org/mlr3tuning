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

test_that("proper error if tuner cannot handle deps", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$add_dep("minsplit", on = "cp", cond = CondEqual$new(0.1))
  term = TerminatorEvals$new(2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, term)
  tt = TunerGenSA$new()
  expect_error(tt$tune(inst), "dependencies")
})
