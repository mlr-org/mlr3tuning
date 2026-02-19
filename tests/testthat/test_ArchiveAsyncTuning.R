skip_if_not_installed("rush")
skip_if_no_redis()

test_that("ArchiveAsyncTuning access methods work", {
  rush = start_rush()
    on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    rush = rush
  )

  expect_benchmark_result(instance$archive$benchmark_result)

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  # benchmark result
  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 20L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)

  # learner
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_learner(instance$archive$learner(i = i))
  })

  # learner param values
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_names(names(instance$archive$learner_param_vals(i)), permutation.of = c("xval", "cp"))
  })

  # learners
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })
})

test_that("ArchiveAsyncTuning as.data.table function works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors")) # FIXME: remove pid and change to permutation.of after rush 1.0.0 is released

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "classif.acc"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "classif.acc", "classif.mcc"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp_xs")
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_ys", "pid", "keys", "warnings", "errors"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp_xs", "resample_result"))
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "timestamp_ys", "pid", "keys", "warnings", "errors"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "x_domain", "keys", "warnings", "errors"))
})

test_that("ArchiveAsyncTuning as.data.table function works without resample result", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE,
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "cp", "classif.ce", "x_domain", "runtime_learners", "worker_id", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors"))
})

test_that("ArchiveAsyncTuning as.data.table function works with empty archive", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE,
    rush = rush
  )

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)
})

test_that("ArchiveAsyncTuning as.data.table function works with new ids in x_domain", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  search_space = ps(
    x1 = p_int(1, 12),
    x2 = p_dbl(0.01, 0.1),
    .extra_trafo = function(x, param_set) {
      x$minsplit = x$x1
      x$cp = x$x2
      x$x1 = NULL
      x$x2 = NULL
      x
    }
  )

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    search_space = search_space,
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "x1", "x2", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors"))
})

test_that("ArchiveAsyncTuning as.data.table function works with switched new ids in x_domain", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  # new ids in x_domain switch
  search_space = ps(
    x1 = p_int(1, 12),
    x2 = p_dbl(0.01, 0.1),
    .extra_trafo = function(x, param_set) {

      if (x$x1 > 3) x$minsplit = x$x1
      x$cp = x$x2
      x$x1 = NULL
      x$x2 = NULL
      x
    }
  )

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    search_space = search_space,
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), subset.of = c("state", "x1", "x2", "classif.ce", "x_domain", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors"))
})

test_that("Saving ArchiveAsyncTuning works", {
  rush = start_rush()
  on.exit({
    file.remove("instance.rds")
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    rush = rush
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  saveRDS(instance, file = "instance.rds")

  loaded_instance = readRDS(file = "instance.rds")

  loaded_instance$reconnect()
  expect_class(loaded_instance, "TuningInstanceAsyncSingleCrit")
})

# Internal Tuning --------------------------------------------------------------

test_that("ArchiveAsyncTuning as.data.table function works internally tuned values", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99),
      x = to_tune(0.1, 0.3)),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    rush = rush
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive, unnest = "x_domain")
  expect_list(tab$internal_tuned_values, min.len = 2, types = "list")
  expect_equal(tab$internal_tuned_values[[1]], set_class(list(iter = 99L), "internal_tuned_values"))

  tab = as.data.table(instance$archive)
  expect_names(names(tab), must.include = "internal_tuned_values_iter")
  expect_equal(tab$internal_tuned_values_iter[1], 99)
})
