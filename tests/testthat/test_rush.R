test_that("tuning with rush works", {
  skip_on_cran()
  skip_on_ci()

  config = start_flush_redis()
  rush = Rush$new("test", config)

  learner = lrn("classif.rpart",
    cp = to_tune(0.01, 0.1)
  )

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_models = FALSE,
    store_benchmark_result = FALSE,
    rush = rush,
    freeze_archive = FALSE
  )

  future::plan("multisession", workers = 2)
  instance$start_workers()
  rush$await_workers(2)

  expect_equal(rush$n_workers, 2)

  tuner = tnr("random_search")

  expect_data_table(tuner$optimize(instance), nrows = 1)
})
