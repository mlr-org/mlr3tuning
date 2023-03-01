test_that("TunerGridSearch", {
  test_tuner("grid_search", resolution = 7, term_evals = 5L, real_evals = 5, n_dim = 1L)
  test_tuner_dependencies("grid_search")

  z = test_tuner("grid_search", resolution = 3, term_evals = 999L, real_evals = 9, n_dim = 2L)
  a = z$inst$archive$data
  expect_data_table(a, nrows = 9)
  expect_set_equal(unique(a$cp), c(0.1, 0.2, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))

  z = test_tuner("grid_search", param_resolutions = c(cp = 2L, minsplit = 3L), term_evals = 999L, real_evals = 6L, n_dim = 2L)
  a = z$inst$archive$data
  expect_data_table(a, nrows = 6L)
  expect_set_equal(unique(a$cp), c(0.1, 0.3))
  expect_set_equal(unique(a$minsplit), c(1, 5, 9))
})

test_that("TunerGridSearch with TerminatorNone", {
  inst = TEST_MAKE_INST1(n_dim = 2L)
  term = TerminatorNone$new()
  tuner = tnr("grid_search", resolution = 2L)
  r = tuner$optimize(inst)
  archive = inst$archive
  expect_data_table(archive$data, nrows = 4L)
})

test_that("TunerGridSearch works with forward hotstart parameter", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))

  instance = tune(
    tuner = tnr( "grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(1, 25, 50, 75, 100))
})

test_that("TunerGridSearch works with forward hotstart parameter", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"

  instance = tune(
    tuner = tnr( "grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(100, 75, 50, 25, 1))
})

test_that("TunerGridSearch works with forward and backward hotstart parameter", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))
  learner$properties = c(learner$properties, "hotstart_backward")

  instance = tune(
    tuner = tnr( "grid_search", batch_size = 5, resolution = 5),
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE,
    allow_hotstart = TRUE
  )

  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(100, 75, 50, 25, 1))
})
