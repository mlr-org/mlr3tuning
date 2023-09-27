# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
# term_evals: how we configure the Terminator
# real_evals: how many evals we really expect (as the optim might early stop)
# returns: tune_result and instance
test_tuner = function(key, ..., n_dim = 1L, term_evals = 2L, real_evals = term_evals) {
  search_space = if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term, search_space)
  tuner = tnr(key, ...)
  expect_tuner(tuner)
  expect_man_exists(tuner$man)
  tuner$optimize(inst)
  archive = inst$archive

  expect_data_table(archive$data, nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  x_opt = inst$result_x_domain
  y_opt = inst$result_y
  expect_list(x_opt, len = n_dim)
  if (n_dim == 1)
    expect_named(x_opt, "cp")
  else
    expect_named(x_opt, c("cp", "minsplit"))
  expect_numeric(y_opt, len = 1L)
  expect_named(y_opt, "classif.ce")
  list(tuner = tuner, inst = inst)
}

# test an implemented subclass tuner by running a test with dependent params
# returns: tune_result and instance
test_tuner_dependencies = function(key, ..., term_evals = 2L) {
  term = trm("evals", n_evals = term_evals)
  ll = LearnerRegrDepParams$new()
  inst = TuningInstanceSingleCrit$new(tsk("boston_housing"), ll, rsmp("holdout"), msr("regr.mse"), term, ll$param_set)
  tuner = tnr(key, ...)
  expect_tuner(tuner)
  tuner$optimize(inst)
  archive = inst$archive

  expect_data_table(archive$data, nrows = term_evals)
  expect_equal(inst$archive$n_evals, term_evals)

  x_opt = inst$result_x_domain
  y_opt = inst$result_y
  expect_list(x_opt)
  expect_names(names(x_opt), subset.of = c("xx", "yy", "cp"))
  expect_numeric(y_opt, len = 1L)
  expect_numeric(y_opt, len = 1L)
  expect_names(names(y_opt), identical.to = "regr.mse")
  list(tuner = tuner, inst = inst)
}
