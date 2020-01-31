context("Tuner")

test_that("API", {
  measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))
  for (n_evals in c(1, 5)) {
    rs = TunerRandomSearch$new()
    inst = TEST_MAKE_INST1(measures = measures, term_evals = n_evals)
    r = rs$tune(inst)
    a = inst$archive(unnest = "params")
    expect_data_table(a, nrows = n_evals)
    expect_true("cp" %in% names(a))
    expect_true("params" %in% names(inst$archive(unnest = "no")))
  }
})

test_that("proper error if tuner cannot handle deps", {
  skip_if_not_installed("GenSA")
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$add_dep("minsplit", on = "cp", cond = CondEqual$new(0.1))
  te = term("evals", n_evals = 2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = TunerGenSA$new()
  expect_error(tt$tune(inst), "dependencies")
})

test_that("we get a result when some subordinate params are not fulfilled", {
  inst = TEST_MAKE_INST2(measures = msr("dummy.cp.regr"))
  d = data.table(xx = c("a", "b"), yy = c(1, NA), cp = c(0.2, 0.1))
  inst$eval_batch(d)
  tuner_assign_result_default(inst)
  r = inst$result
  expect_equal(r$perf, c(dummy.cp.regr = 0.1))
  expect_equal(r$tune_x, list(xx = "b", cp = 0.1))
})

