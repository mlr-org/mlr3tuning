test_that("resample results are discarded after tuning", {
  task = tsk("pima")
  learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100))

  instance = tune(
    method = "random_async",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 10
  )

  expect_null(instance$archive$data$resample_result)
})
