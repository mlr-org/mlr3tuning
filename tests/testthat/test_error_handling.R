context("error handling")

test_that("failing learner", {
  learner = lrn("classif.debug")
  param_set = ParamSet$new(list(
      ParamDbl$new("x", lower = 0, upper = 1)
  ))
  learner$param_set$values$error_train = 0.5

  tt = tnr("random_search")

  instance = TuningInstance$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measures = msr("classif.ce"), param_set = param_set, terminator = term("evals", n_evals = 10))
  expect_error(tt$tune(instance), "classif.debug->train")

  learner$fallback = lrn("classif.featureless")
  learner$encapsulate = c (train = "evaluate", predict = "evaluate")

  instance = TuningInstance$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measures = msr("classif.ce"), param_set = param_set, terminator = term("evals", n_evals = 10))
  tt$tune(instance)
  res = tt$tune_result(instance)
  expect_list(res, len = 2)
})


test_that("predictions missing", {
  learner = lrn("classif.debug")
  param_set = ParamSet$new(list(
      ParamDbl$new("x", lower = 0, upper = 1)
  ))
  learner$param_set$values$predict_missing = 0.5

  tt = tnr("random_search")

  instance = TuningInstance$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measures = msr("classif.ce"), param_set = param_set, terminator = term("evals", n_evals = 10))
  tt$tune(instance)
  expect_error(tt$tune_result(instance), "[Nn]o non-missing")
})


test_that("faulty measure", {
  learner = lrn("classif.debug")
  tt = tnr("random_search")
  param_set = ParamSet$new(list(
      ParamDbl$new("x", lower = 0, upper = 1)
  ))

  instance = TuningInstance$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measures = msr("debug", na_ratio = 0.5, minimize = TRUE), param_set = param_set, terminator = term("evals", n_evals = 10))
  tt$tune(instance)
  tab = instance$archive()
  expect_data_table(tab, nrows = 10)
  expect_numeric(tab$debug)
  expect_gt(sum(is.na(tab$debug)), 0)

  rr = instance$best()
  expect_resample_result(rr)
})
