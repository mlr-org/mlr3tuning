test_that("hotstart works", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))

  instance = tune(
    method = "grid_search",
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    batch_size = 5,
    resolution = 5,
    allow_hotstart = TRUE
  )

  expect_r6(instance, "TuningInstanceSingleCrit")
})
