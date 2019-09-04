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
  expect_list(instance$selected_config, len = 2)
  expect_named(instance$selected_config, c("error_train", "x"))
})
