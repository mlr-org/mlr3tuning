test_that("initializing TuningInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(instance, "TuningInstanceAsyncMultiCrit")
  expect_r6(instance$archive, "ArchiveAsyncTuning")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$terminator, "Terminator")
  expect_r6(instance$rush, "Rush")
  expect_null(instance$result)

  expect_rush_reset(instance$rush, type = "terminate")
})

test_that("rush controller can be passed to TuningInstanceAsyncSingleCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush = rsh(network_id = "remote_network")

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(instance, "TuningInstanceAsyncMultiCrit")
  expect_class(instance$rush, "Rush")
  expect_equal(instance$rush$network_id, "remote_network")

  expect_rush_reset(instance$rush, type = "terminate")
})

test_that("TuningInstanceAsyncSingleCrit can be passed to a tuner", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3)
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 3L)
  expect_rush_reset(instance$rush, type = "terminate")
})

test_that("assigning a result to TuningInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3)
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  result = instance$result
  expect_data_table(result, min.rows = 1)
  expect_names(names(result), identical.to = c("cp", "learner_param_vals", "x_domain", "classif.ce", "classif.acc"))
})

test_that("saving the benchmark result with TuningInstanceRushSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)
})

test_that("saving the models with TuningInstanceRushSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_class(instance$archive$resample_result(1)$learners[[1]]$model, "rpart")
})

# test_that("crashing workers are detected", {
#   skip_on_cran()
#   skip_if_not_installed("rush")
#   flush_redis()

#   rush_plan(n_workers = 2, start_worker_timeout = 10)

#   learner = lrn("classif.debug", segfault_train = 0.2, x = to_tune())
#   learner$encapsulate = c(train = "callr")

#   instance = ti_async(
#     task = tsk("pima"),
#     learner = learner,
#     resampling = rsmp("cv", folds = 3),
#     measures = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 100)
#   )

#   tuner = tnr("async_random_search")

#   tuner$optimize(instance)
# })

