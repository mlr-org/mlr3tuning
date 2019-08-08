lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_terminator = function(term) {
  expect_r6(term, "Terminator", public = c("eval_before", "eval_after", "remaining"))
  expect_flag(term$terminated)
  expect_string(term$remaining)
}

# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
test_tuner = function(tuner_factory, arg_list = list(), n_evals = 5L) {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1)
  ))
  pe = PerfEval$new("iris", "classif.rpart", "holdout", "classif.ce", ps)
  term = TerminatorEvaluations$new(n_evals)
  arg_list_2 = list(pe = pe, terminator = term)
  arg_list_2 = insert_named(arg_list_2, arg_list)
  tuner = do.call(tuner_factory$new, arg_list_2)

  tuner$tune()
  r = tuner$tune_result()
  bmr = tuner$pe$bmr

  expect_data_table(bmr$data, nrows = n_evals)
  expect_equal(pe$n_evals, n_evals)
  expect_list(r)
  expect_number(r$performance["classif.ce"], lower = 0, upper = 1)
  expect_list(r$values, len = 2L)
}




