context("PerformanceEvaluator")

test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("holdout")
  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures
  param_set = paradox::ParamSet$new(params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  pe = PerformanceEvaluator$new(
    task = task,
    learner = learner,
    resampling = resampling,
    param_set = param_set,
    ctrl = tune_control(store_prediction = TRUE) # for the exceptions
  )

  expect_r6(pe, "PerformanceEvaluator")
  expect_r6(pe$param_set, "ParamSet")
  expect_null(pe$bmr)

  expect_r6(pe$eval(data.table::data.table(cp = c(0.01, 0.02))), "PerformanceEvaluator")
  expect_data_table(pe$bmr$data, nrow = 2L)
  expect_equal(pe$bmr$data$learner[[1L]]$param_set$values$cp, 0.01)
  expect_equal(pe$bmr$data$learner[[1L]]$param_set$values$minsplit, 3)
  expect_equal(pe$bmr$data$learner[[2L]]$param_set$values$cp, 0.02)

  expect_r6(pe$eval(data.table(cp = 0.1)), "PerformanceEvaluator")
  expect_data_table(pe$bmr$data, nrow = 3L)
  expect_equal(pe$bmr$data$learner[[3L]]$param_set$values$cp, 0.1)
  expect_equal(pe$bmr$data$learner[[3L]]$param_set$values$minsplit, 3)

  expect_resample_result(pe$get_best())
})


test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("holdout")
  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures
  param_set = paradox::ParamSet$new(params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  pe = PerformanceEvaluator$new(
    task = task,
    learner = learner,
    resampling = resampling,
    param_set = param_set,
    ctrl = tune_control(store_prediction = TRUE) # for the exceptions
  )

  expect_error(pe$add_hook(hook = mean))
  expect_silent(pe$add_hook(hook = function(pe) {
    "hook"
  }))
  expect_equal(pe$run_hooks(), list("hook"))
})
