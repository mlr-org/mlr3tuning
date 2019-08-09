lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_terminator = function(term) {
  expect_r6(term, "Terminator", public = c("eval_before", "eval_after"))
  expect_flag(term$terminated)
}

# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
# term_evals: how we configure the Terminator
# real_evals: how many evals we really expect (as the optim might early stop)
# returns: tuner, so we can investgate its state more in individual tests
test_tuner = function(tuner_factory, arg_list = list(), n_dim = 1L,
  term_evals = 5L, real_evals = term_evals) {

  ps = if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
  pe = PerfEval$new("iris", "classif.rpart", "holdout", "classif.ce", ps)
  term = TerminatorEvaluations$new(term_evals)
  arg_list_2 = list(pe = pe, terminator = term)
  arg_list_2 = insert_named(arg_list_2, arg_list)
  tuner = do.call(tuner_factory$new, arg_list_2)

  tuner$tune()
  r = tuner$tune_result()
  bmr = tuner$pe$bmr

  expect_data_table(bmr$data, nrows = real_evals)
  expect_equal(pe$n_evals, real_evals)
  expect_list(r)
  expect_number(r$performance["classif.ce"], lower = 0, upper = 1)
  expect_list(r$values, len = n_dim + 1)
  return(tuner)
}




