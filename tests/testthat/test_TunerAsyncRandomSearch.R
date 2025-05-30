test_that("TunerAsyncRandomSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, min.rows = 20)
  expect_rush_reset(instance$rush, type = "kill")
})


