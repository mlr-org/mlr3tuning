test_that("failing learner", {
  learner = lrn("classif.debug")
  param_set = ps(
      x = p_dbl(lower = 0, upper = 1)
  )
  learner$param_set$values$error_train = 0.5

  tt = tnr("random_search")

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), search_space = param_set, terminator = trm("evals", n_evals = 10))
  expect_resample_error(tt$optimize(instance), "classif.debug->train")

  learner$encapsulate("evaluate", lrn("classif.featureless"))

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), search_space = param_set, terminator = trm("evals", n_evals = 10))
  tt$optimize(instance)
  rc = expect_list(instance$result_x_domain)
  expect_list(rc, len = 1)
  expect_named(rc, c("x"))
})


test_that("predictions missing", {
  learner = lrn("classif.debug")
  param_set = ps(
      x = p_dbl(lower = 0, upper = 1)
  )
  learner$param_set$values$predict_missing = 0.5

  tt = tnr("random_search")

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), search_space = param_set, terminator = trm("evals", n_evals = 10))
  expect_resample_error(tt$optimize(instance), "missing")
})
