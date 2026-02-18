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
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(instance, "TuningInstanceAsyncMultiCrit")
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
    measures = msrs(c("classif.ce", "classif.acc")),
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
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  result = instance$result
  expect_data_table(result, min.rows = 1)
  expect_names(names(result), must.include = c("cp", "learner_param_vals", "x_domain", "classif.ce", "classif.acc"))
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
    measures = msrs(c("classif.ce", "classif.acc")),
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
    measures = msrs(c("classif.ce", "classif.acc")),
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

#   rush::rush_plan(n_workers = 2, start_worker_timeout = 10)

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


# Internal Tuning --------------------------------------------------------------

test_that("Multi-crit internal tuning works", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  learner = lrn("classif.debug",
    iter = to_tune(upper = 1000L, internal = TRUE, aggr = function(x) as.integer(ceiling(mean(unlist(x))) + 2000L)),
    x = to_tune(0.2, 0.3),
    predict_type = "prob",
    validate = 0.3,
    early_stopping = TRUE
  )
  # this ensures we get a pareto front that contains all values
  m1 = msr("classif.acc")
  m2 = msr("classif.acc", id = "classif.acc2")
  m2$minimize = TRUE

  instance = ti_async(
    learner = learner,
    task = tsk("sonar"),
    resampling = rsmp("cv", folds = 2L),
    measures = list(m1, m2),
    terminator = trm("evals", n_evals = 20),
    rush = rush
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), min.rows = 20)

  expect_list(instance$result_learner_param_vals, min.len = 20L)
  expect_list(instance$archive$finished_data$internal_tuned_values, min.len = 20L)
  expect_true(all(map_int(instance$archive$finished_data$internal_tuned_values, "iter") >= 2000L))
  expect_true(all(map_lgl(instance$result_learner_param_vals, function(x) x$iter >= 2000L)))
  expect_true(length(unique(map_int(instance$archive$finished_data$internal_tuned_values, "iter"))) > 1L)

  expect_permutation(
    map_int(instance$result_learner_param_vals, "iter")[1:20],
    map_int(instance$archive$finished_data$internal_tuned_values, "iter")[1:20]
  )
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
    measures = msrs(c("classif.ce", "classif.acc")),
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
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  optimizer = tnr("async_random_search")
  expect_data_table(optimizer$optimize(instance))
})
