skip_if_not_installed("rush")
skip_if_no_redis()

test_that("initializing TuningInstanceAsyncSingleCrit works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_r6(instance$archive, "ArchiveAsyncTuning")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$terminator, "Terminator")
  expect_r6(instance$rush, "Rush")
  expect_null(instance$result)
})

test_that("TuningInstanceAsyncSingleCrit can be passed to a tuner", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 3L)
})

test_that("assigning a result to TuningInstanceAsyncSingleCrit works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  result = instance$result
  expect_data_table(result, nrows = 1)
  expect_names(names(result), must.include = c("cp", "learner_param_vals", "x_domain", "classif.ce"))
})

test_that("saving the benchmark result with TuningInstanceRushSingleCrit works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    rush = rush
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)
})

test_that("saving the models with TuningInstanceRushSingleCrit works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
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

#   mirai::daemons(2)
#   rush::rush_plan(n_workers = 2, worker_type = "remote")

#   instance = ti_async(
#     task = tsk("pima"),
#     learner = lrn("classif.debug", segfault_train = 1, x = to_tune()),
#     resampling = rsmp("cv", folds = 3),
#     measures = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 10)
#   )

#   tuner = tnr("async_random_search")

#   tuner$optimize(instance)
# })

# Internal Tuning --------------------------------------------------------------

test_that("Async single-crit internal tuning works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, x = to_tune(0.2, 0.3),
    iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99))

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    rush = rush
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_list(instance$archive$finished_data$internal_tuned_values, min.len = 20, types = "list")
  expect_equal(instance$archive$finished_data$internal_tuned_values[[1]], set_class(list(iter = 99L), "internal_tuned_values"))
  expect_false(instance$result_learner_param_vals$early_stopping)
  expect_equal(instance$result_learner_param_vals$iter, 99)
})

test_that("Internal tuning throws an error on incorrect configuration", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  expect_error(tune(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE)),
    task = tsk("iris"),
    resampling = rsmp("holdout"),
    rush = rush
  ), "early_stopping")
})

test_that("Internal tuning throws an error message when primary search space is empty", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  expect_error(tune(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE), early_stopping = TRUE, validate = 0.2),
    task = tsk("iris"),
    resampling = rsmp("holdout"),
    rush = rush
  ), "tnr('internal')", fixed = TRUE)
})

test_that("tiny logging works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  old_opts = options(bbotk.tiny_logging = TRUE)
  on.exit(options(old_opts), add = TRUE)

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  optimizer = tnr("async_random_search")
  expect_data_table(optimizer$optimize(instance))
})

test_that("tiny logging work with internal tuning", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  old_opts = options(bbotk.tiny_logging = TRUE)
  on.exit(options(old_opts), add = TRUE)

  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, x = to_tune(0.2, 0.3),
    iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99))

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  optimizer = tnr("async_random_search")
  expect_data_table(optimizer$optimize(instance))
})
