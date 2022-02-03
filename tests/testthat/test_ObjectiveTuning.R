test_that("ObjectiveTuning", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, archive = archive)

  expect_true("noisy" %in% obj$properties)
  expect_equal(obj$id, "classif.rpart_on_iris")

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 5)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(obj$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$cp,  0.02)

  xss = list(list("cp" = 0.01, minsplit = 3), list("cp" = 0.02, minsplit = 4))
  z = obj$eval_many(xss)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(obj$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(obj$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$cp, 0.02)
  expect_equal(obj$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$minsplit, 4)
})

test_that("ObjectiveTuning - Multiple measures", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("classif.ce", "classif.acc"))

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, archive = archive)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))
  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 6)
})

test_that("ObjectiveTuning - Store models", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, store_models = TRUE, archive = archive)

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

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, archive = archive)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 5)
  expect_named(z, c("classif.ce", "runtime_learners", "uhash", "warnings", "errors"), ignore.order = TRUE)

  t1 = sum(map_dbl(obj$archive$benchmark_result$resample_result(1)$learners, function(l) sum(l$timings)))
  t2 = sum(map_dbl(obj$archive$benchmark_result$resample_result(2)$learners, function(l) sum(l$timings)))
  expect_equal(z[1, runtime_learners], t1)
  expect_equal(z[2, runtime_learners], t2)

  # repeated cv
  resampling = rsmp("repeated_cv", repeats = 3, folds =3)

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, archive = archive)

  xss = list(list("cp" = 0.01), list("cp" = 0.02))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 5)
  expect_named(z, c("classif.ce", "runtime_learners", "uhash", "warnings", "errors"), ignore.order = TRUE)

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
})

test_that("benchmark clone works", {
  grid = benchmark_grid(
    tasks = tsk("iris"),
    learners = lrn("classif.featureless"),
    resamplings = rsmp("holdout")
  )
  task = grid$task[[1L]]
  learner = grid$learner[[1L]]
  resampling = grid$resampling[[1L]]

  bmr = benchmark(grid, clone = c())

  expect_same_address(task, bmr$tasks$task[[1]])
  expect_same_address(learner, get_private(bmr)$.data$data$learners$learner[[1]])
  expect_same_address(resampling, bmr$resamplings$resampling[[1]])

  expect_identical(task$hash, bmr$tasks$task[[1]]$hash)
  expect_identical(learner$hash, bmr$learners$learner[[1]]$hash)
  expect_identical(resampling$hash, bmr$resamplings$resampling[[1]]$hash)
})

test_that("objects are cloned", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  archive = ArchiveTuning$new(search_space = learner$param_set, codomain = measures_to_codomain(measures))
  obj = ObjectiveTuning$new(task, learner, resampling, measures, archive = archive)

  xss = list(list("cp" = 0.01, minsplit = 3), list("cp" = 0.02, minsplit = 4))
  z = obj$eval_many(xss)
  bmr = archive$benchmark_result

  expect_same_address(obj$task, bmr$tasks$task[[1]])
  expect_different_address(obj$learner, get_private(bmr)$.data$data$learners$learner[[1]])
  expect_same_address(obj$resampling, bmr$resamplings$resampling[[1]])
})
