test_that("ObjectiveTuningAsync returns resample result with model", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  resampling = rsmp("holdout")
  measures = msr("classif.ce")

  objective = ObjectiveTuningAsync$new(task, learner, resampling, measures, store_benchmark_result = TRUE,
    store_models = TRUE, allow_hotstart = FALSE)

  expect_null(objective$hotstart_stack)

  xs = list("x" = 0.01)
  z = objective$eval(xs)
  expect_list(z, len = 3)
  expect_names(names(z), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "classif.debug_model")
})

test_that("ObjectiveTuningAsync returns resample result with model when hotstarting is activated", {
  objective = ObjectiveTuningAsync$new(task, learner, resampling, measures, store_benchmark_result = TRUE,
    store_models = TRUE, allow_hotstart = TRUE)

  expect_true(objective$allow_hotstart)
  expect_r6(objective$hotstart_stack, "HotstartStack")

  xs = list("x" = 0.01, iter = 5)
  z = objective$eval(xs)
  expect_list(z, len = 3)
  expect_names(names(z), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "classif.debug_model")
})

test_that("ObjectiveTuningAsync returns resample result with model when store models is false but hotstarting is activated", {
  objective = ObjectiveTuningAsync$new(task, learner, resampling, measures, store_benchmark_result = TRUE,
    store_models = FALSE, allow_hotstart = TRUE)

  expect_true(objective$allow_hotstart)
  expect_r6(objective$hotstart_stack, "HotstartStack")

  xs = list("x" = 0.01, iter = 5)
  z = objective$eval(xs)
  expect_list(z, len = 3)
  expect_names(names(z), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "classif.debug_model")
})

test_that("ObjectiveTuningAsync returns resample result with model when store benchmark result is false but hotstarting is activated", {
  objective = ObjectiveTuningAsync$new(task, learner, resampling, measures, store_benchmark_result = FALSE,
    store_models = TRUE, allow_hotstart = TRUE)

  expect_true(objective$allow_hotstart)
  expect_r6(objective$hotstart_stack, "HotstartStack")

  xs = list("x" = 0.01, iter = 5)
  z = objective$eval(xs)
  expect_list(z, len = 3)
  expect_names(names(z), permutation.of = c("classif.ce", "runtime_learners", "resample_result"))
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "classif.debug_model")
})

test_that("ObjectiveTuningAsync returns no resample result", {
  objective = ObjectiveTuningAsync$new(task, learner, resampling, measures, store_benchmark_result = FALSE,
    store_models = FALSE, allow_hotstart = FALSE)

  expect_false(objective$allow_hotstart)
  expect_null(objective$hotstart_stack)

  xs = list("x" = 0.01, iter = 5)
  z = objective$eval(xs)
  expect_list(z, len = 2)
  expect_names(names(z), permutation.of = c("classif.ce", "runtime_learners"))
})
