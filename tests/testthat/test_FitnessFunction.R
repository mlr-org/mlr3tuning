context("FitnessFunction")

test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_vals = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("holdout")
  measures = mlr3::mlr_measures$mget("mmce")
  param_set = paradox::ParamSet$new(params = list(paradox::ParamReal$new("cp", lower = 0.001, upper = 0.1)))

  ff = FitnessFunction$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = param_set,
    ctrl = tune_control(store_prediction = TRUE) # for the exceptions
  )

  expect_r6(ff, "FitnessFunction")
  expect_r6(ff$param_set, "ParamSet")

  expect_r6(ff$eval(list(cp = 0.01)), "FitnessFunction")
  expect_data_table(ff$experiments, nrow = 1L)
  expect_equal(ff$experiments$learner[[1L]]$param_vals$cp, 0.01)
  expect_equal(ff$experiments$learner[[1L]]$param_vals$minsplit, 3)

  expect_r6(ff$eval(list(cp = 0.1)), "FitnessFunction")
  expect_data_table(ff$experiments, nrow = 2L)
  expect_equal(ff$experiments$learner[[2L]]$param_vals$cp, 0.1)
  expect_equal(ff$experiments$learner[[2L]]$param_vals$minsplit, 3)

  expect_resample_result(ff$get_best())
})
