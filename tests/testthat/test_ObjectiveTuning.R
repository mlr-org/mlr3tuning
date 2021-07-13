test_that("ObjectiveTuning", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  expect_equal(obj$id, "classif.rpart_on_iris")

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp,
    0.01)
  expect_equal(obj$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$cp,
    0.02)

  xss = list(list("cp" = 0.01, minsplit = 3), list("cp" = 0.02, minsplit = 4))
  z = obj$eval_many(xss)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(
    obj$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$minsplit,
    3)
  expect_equal(obj$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$cp, 0.02)
  expect_equal(
    obj$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$minsplit,
    4)
})

test_that("ObjectiveTuning - Multiple measures", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("classif.ce", "classif.acc"))

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 4)
})

test_that("ObjectiveTuning - Store models", {
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
  expect_class(as.data.table(obj$archive$benchmark_result)$learner[[1]]$model, "rpart")
})


test_that("runtime of learners is added", {
  # cv
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds =3)
  measures = msr("classif.ce")

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
  expect_named(z, c("classif.ce", "runtime_learners", "uhash"))

  t1 = sum(map_dbl(obj$archive$benchmark_result$resample_result(1)$learners, function(l) sum(l$timings)))
  t2 = sum(map_dbl(obj$archive$benchmark_result$resample_result(2)$learners, function(l) sum(l$timings)))
  expect_equal(z[1, runtime_learners], t1)
  expect_equal(z[2, runtime_learners], t2)


  # repeated cv
  resampling = rsmp("repeated_cv", repeats = 3, folds =3)

  obj = ObjectiveTuning$new(
    task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
  expect_named(z, c("classif.ce", "runtime_learners", "uhash"))

  t1 = sum(map_dbl(obj$archive$benchmark_result$resample_result(1)$learners, function(l) sum(l$timings)))
  t2 = sum(map_dbl(obj$archive$benchmark_result$resample_result(2)$learners, function(l) sum(l$timings)))
  expect_equal(z[1, runtime_learners], t1)
  expect_equal(z[2, runtime_learners], t2)
})

test_that("tuner can modify resampling", {
  instance = TuningInstanceSingleCrit$new(
    task = tsk("iris"),
    learner = lrn("classif.rpart", cp = to_tune(0.001, 0.1)),
    resampling = rsmp("cv", folds =3),
    measure = msr("classif.ce"),
    terminator = trm("none")
  )

  instance$eval_batch(data.table(cp = 0.001))
  rr = instance$archive$resample_result(1) 
  expect_equal(rr$resampling$id, "cv")

  # add new resampling
  new_resampling = rsmp("holdout")
  new_resampling$instantiate(tsk("iris"))
  instance$objective$constants$values$resampling = list(new_resampling)
  instance$eval_batch(data.table(cp = 0.001))
  rr = instance$archive$resample_result(2) 
  expect_equal(rr$resampling$id, "holdout")

  # uninstantiated resampling
  new_resampling = rsmp("cv", folds = 2)
  instance$objective$constants$values$resampling = list(new_resampling)
  expect_error(instance$eval_batch(data.table(cp = 0.001)), regex = "must be instantiated")
})
