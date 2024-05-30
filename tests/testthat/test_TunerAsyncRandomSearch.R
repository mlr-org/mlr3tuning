test_that("TunerAsyncRandomSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  rush_plan(n_workers = 2)
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
  expect_rush_reset(instance$rush, type = "terminate")
})

test_that("internal tuning is supported", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, x = to_tune(0.2, 0.3),
    iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)
  expect_equal(
    instance$archive$data$internal_tuned_values,
    replicate(list(list(iter = 99)), n = length(instance$archive$data$internal_tuned_values)),
  )
  expect_false(instance$result_learner_param_vals$early_stopping)
  expect_equal(instance$result_learner_param_vals$iter, 99)
})
