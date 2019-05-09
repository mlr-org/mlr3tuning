context("FitnessFunction")

test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("holdout")
  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures
  param_set = paradox::ParamSet$new(params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  ff = FitnessFunction$new(
    task = task,
    learner = learner,
    resampling = resampling,
    param_set = param_set,
    ctrl = tune_control(store_prediction = TRUE) # for the exceptions
  )

  expect_r6(ff, "FitnessFunction")
  expect_r6(ff$param_set, "ParamSet")
  expect_null(ff$bmr)

  expect_r6(ff$eval(data.table::data.table(cp = c(0.01, 0.02))), "FitnessFunction")
  expect_data_table(ff$bmr$data, nrow = 2L)
  expect_equal(ff$bmr$data$learner[[1L]]$param_set$values$cp, 0.01)
  expect_equal(ff$bmr$data$learner[[1L]]$param_set$values$minsplit, 3)
  expect_equal(ff$bmr$data$learner[[2L]]$param_set$values$cp, 0.02)

  expect_r6(ff$eval(data.table(cp = 0.1)), "FitnessFunction")
  expect_data_table(ff$bmr$data, nrow = 3L)
  expect_equal(ff$bmr$data$learner[[3L]]$param_set$values$cp, 0.1)
  expect_equal(ff$bmr$data$learner[[3L]]$param_set$values$minsplit, 3)

  expect_resample_result(ff$get_best())
})


test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("holdout")
  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures
  param_set = paradox::ParamSet$new(params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  ff = FitnessFunction$new(
    task = task,
    learner = learner,
    resampling = resampling,
    param_set = param_set,
    ctrl = tune_control(store_prediction = TRUE) # for the exceptions
  )

  expect_error(ff$add_hook(hook = mean))
  expect_silent(ff$add_hook(hook = function(ff) {
    "hook"
  }))
  expect_equal(ff$run_hooks(), list("hook"))
})
