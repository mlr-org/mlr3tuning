test_that("hotstart works forwards", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 101))

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(1, 26, 51, 76, 101))
  expect_null(instance$archive$data$resample_result)
})

test_that("hotstart works backwards", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 101))
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(101, 76, 51, 26, 1))
  expect_null(instance$archive$data$resample_result)
})

test_that("hotstart works forwards and backwards", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 101))
  learner$properties = c(learner$properties, "hotstart_backward")

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(101, 76, 51, 26, 1))
  expect_null(instance$archive$data$resample_result)
})

test_that("hotstart flag is not set to TRUE if learners does not support hotstarting", {
  task = tsk("pima")
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  expect_false(instance$objective$allow_hotstart)
})

test_that("models are discarded after storing them in the stack", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 101))

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = FALSE,
    allow_hotstart = TRUE,
    keep_hotstart_stack = TRUE
  )

  expect_null(instance$archive$data$resample_result)
  expect_null(instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)
  expect_list(instance$objective$hotstart_stack$stack$start_learner, len = 25)
  expect_learner(instance$objective$hotstart_stack$stack$start_learner[[1]])
  expect_class(instance$objective$hotstart_stack$stack$start_learner[[1]]$model, "classif.debug_model")
})


test_that("objects are cloned", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 101))

  instance = tune(
    tuner = tnr("grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    allow_hotstart = TRUE,
    keep_hotstart_stack = TRUE
  )

  bmr = instance$archive$benchmark_result
  expect_different_address(instance$objective$learner, get_private(bmr)$.data$data$learners$learner[[1]])
  expect_character(map_chr(instance$objective$hotstart_stack$stack$start_learner, data.table::address), unique = TRUE, len = 25)
  walk(instance$objective$hotstart_stack$stack$start_learner, function(learner) {
      expect_different_address(learner, instance$objective$learner)
      expect_different_address(learner$param_set, instance$objective$learner$param_set)
      expect_different_address(learner, get_private(bmr)$.data$data$learners$learner[[1]])
      expect_different_address(learner$param_set, get_private(bmr)$.data$data$learners$learner[[1]]$param_set)
  })
})
