test_that("objective async works", {
  objective = ObjectiveTuningAsync$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    store_models = FALSE,
    store_benchmark_result = FALSE
  )

  xs = list("cp" = 0.01)
  y = objective$eval(xs)

  expect_list(y, len = 2)
  expect_names(names(y), permutation.of = c("classif.ce", "runtime_learners", "warnings", "errors", "logs"))
  expect_number(y$classif.ce)
  expect_number(y$runtime_learners)
})

test_that("store benchmark result works", {
  objective = ObjectiveTuningAsync$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    store_models = FALSE,
    store_benchmark_result = TRUE
  )

  xs = list("cp" = 0.01)
  y = objective$eval(xs)

  expect_list(y, len = 3)
  expect_names(names(y), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_number(y$classif.ce)
  expect_number(y$runtime_learners)
  expect_resample_result(y$resample_result[[1]])
  expect_null(y$resample_result[[1]]$learners[[1]]$model)
})

test_that("store models works", {
  objective = ObjectiveTuningAsync$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    store_models = TRUE,
    store_benchmark_result = TRUE
  )

  xs = list("cp" = 0.01)
  y = objective$eval(xs)

  expect_list(y, len = 3)
  expect_names(names(y), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_number(y$classif.ce)
  expect_number(y$runtime_learners)
  expect_resample_result(y$resample_result[[1]])
  expect_class(y$resample_result[[1]]$learners[[1]]$model, "rpart")
})

test_that("rush objective with multiple measures works", {
  objective = ObjectiveTuningAsync$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    store_models = FALSE,
    store_benchmark_result = FALSE
  )

  xs = list("cp" = 0.01)
  y = objective$eval(xs)

  expect_list(y, len = 3)
  expect_names(names(y), permutation.of = c("classif.ce", "classif.acc", "runtime_learners"))
  expect_number(y$classif.ce)
  expect_number(y$classif.acc)
  expect_number(y$runtime_learners)
})
