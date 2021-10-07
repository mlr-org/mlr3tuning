test_that("hotstart works", {
  task = tsk("pima")

  # forward
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))

  instance = tune(
    method = "grid_search",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    batch_size = 5,
    resolution = 5,
    allow_hotstart = TRUE
  )

  ids = map(extract_bmr_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
    expect_equal(unique(instance$archive$data$iter), c(1, 25, 50, 75, 100))

  # backward
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"

  instance = tune(
    method = "grid_search",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    batch_size = 5,
    resolution = 5,
    allow_hotstart = TRUE
  )

  ids = map(extract_bmr_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(100, 75, 50, 25, 1))

  # forward and backward
  learner$properties = c(learner$properties, "hotstart_backward")

  instance = tune(
    method = "grid_search",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    batch_size = 5,
    resolution = 5,
    allow_hotstart = TRUE
  )

  ids = map(extract_bmr_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(100, 75, 50, 25, 1))
})
