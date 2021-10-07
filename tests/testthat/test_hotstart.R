test_that("hotstart works", {
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

  ids = map(extract_bmr_learners(instance$archive$benchmark_result), function(l) {
    l$model$id
  })

  expect_equal(length(unique(ids)), 5)
})
