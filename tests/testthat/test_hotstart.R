test_that("hotstart works forwards", {
  task = tsk("pima")
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
})

test_that("hotstart works backwards", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))
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
})

test_that("hotstart works forwards and backwards", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))
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

test_that("hotstart flag is not set to TRUE if learners does not support hotstarting", {
  task = tsk("pima")
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))

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

  expect_false(instance$objective$allow_hotstart)
})
