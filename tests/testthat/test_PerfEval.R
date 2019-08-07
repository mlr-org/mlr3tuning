context("PerfEval")

test_that("PerfEval", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr_resamplings$get("holdout")
  measures = mlr_measures$mget("classif.ce")
  param_set = ParamSet$new(params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  pe = PerfEval$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = param_set
  )

  # test empty PE
  expect_null(pe$bmr)
  expect_identical(pe$n_evals, 0L)

  # add a couple of eval points and test the state of PE
  pe$eval(data.table(cp = c(0.01, 0.02)))
  expect_data_table(pe$bmr$data, nrows = 2L)
  expect_equal(pe$bmr$data$learner[[1L]]$param_set$values$cp, 0.01)
  expect_equal(pe$bmr$data$learner[[1L]]$param_set$values$minsplit, 3)
  expect_equal(pe$bmr$data$learner[[2L]]$param_set$values$cp, 0.02)
  expect_identical(pe$n_evals, 2L)

  pe$eval(data.table(cp = 0.1))
  expect_data_table(pe$bmr$data, nrows = 3L)
  expect_equal(pe$bmr$data$learner[[3L]]$param_set$values$cp, 0.1)
  expect_equal(pe$bmr$data$learner[[3L]]$param_set$values$minsplit, 3)
  expect_identical(pe$n_evals, 3L)

  expect_resample_result(pe$best())
})


test_that("hooks", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr_resamplings$get("holdout")
  measures = mlr_measures$mget("classif.ce")
  param_set = ParamSet$new(params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1)))

  pe = PerfEval$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = param_set
  )

  expect_error(pe$add_hook(hook = mean))
  expect_silent(pe$add_hook(hook = function(pe) {
    "hook"
  }))
  expect_equal(pe$run_hooks(), list("hook"))
})
