skip_if_not_installed("rush")
skip_if_no_redis()

test_that("TunerAsyncDesignPoints works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("none"),
    store_benchmark_result = FALSE,
    rush = rush
  )

  design = data.table(cp = c(0.01, 0.1), minsplit = c(2, 4))
  tuner = tnr("async_design_points", design = design)
  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, nrows = 2)
})
