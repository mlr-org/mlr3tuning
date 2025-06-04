test_that("TunerAsyncGridSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  rush::rush_plan(n_workers = 2, worker_type = "remote")
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("none"),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_grid_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, nrows = 100)
  expect_rush_reset(instance$rush, type = "kill")
})
