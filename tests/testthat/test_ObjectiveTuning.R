context("ObjectiveTuning")

test_that("ObjectiveTuning", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 2)
  expect_equal(z$resample_result[[1]]$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(z$resample_result[[2]]$learners[[1]]$param_set$values$cp, 0.02)

  xss = list(list("cp" = 0.01, minsplit = 3), list("cp" = 0.02, minsplit = 4))
  z = obj$eval_many(xss)
  expect_equal(z$resample_result[[1]]$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(
    z$resample_result[[1]]$learners[[1]]$param_set$values$minsplit,
    3)
  expect_equal(z$resample_result[[2]]$learners[[1]]$param_set$values$cp, 0.02)
  expect_equal(
    z$resample_result[[2]]$learners[[1]]$param_set$values$minsplit,
    4)
})

test_that("ObjectiveFSelect - Multiple measures", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("classif.ce", "classif.acc"))

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
})

test_that("ObjectiveFSelect - Store models", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures,
    store_models = TRUE)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))

  z = obj$eval_many(xss)
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "rpart")
})