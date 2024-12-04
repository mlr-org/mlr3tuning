test_that("ArchiveAsyncTuningFrozen works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  archive = instance$archive
  frozen_archive = ArchiveAsyncTuningFrozen$new(archive)

  expect_data_table(frozen_archive$data)
  expect_data_table(frozen_archive$queued_data)
  expect_data_table(frozen_archive$running_data)
  expect_data_table(frozen_archive$finished_data)
  expect_data_table(frozen_archive$failed_data)
  expect_number(frozen_archive$n_queued)
  expect_number(frozen_archive$n_running)
  expect_number(frozen_archive$n_finished)
  expect_number(frozen_archive$n_failed)
  expect_number(frozen_archive$n_evals)
  expect_benchmark_result(frozen_archive$benchmark_result)

  expect_data_table(as.data.table(frozen_archive))
})
