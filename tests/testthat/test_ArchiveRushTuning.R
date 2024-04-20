test_that("ArchiveAsyncTuning access methods work", {
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
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )
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
    expect_named(instance$archive$learner_param_vals(i), c("xval", "minsplit", "cp"))
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

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncTuning as.data.table function works", {
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
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20, ncols = 15)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, min.rows = 20, ncols = 16)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log", "classif.acc"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, min.rows = 20, ncols = 17)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log", "classif.acc", "classif.mcc"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp_xs")
  expect_data_table(tab, min.rows = 20, ncols = 14)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "resample_result", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp_xs", "resample_result"))
  expect_data_table(tab, min.rows = 20, ncols = 13)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, min.rows = 20, ncols = 15)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, min.rows = 20, ncols = 14)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "x_domain", "keys", "warnings", "errors", "log"))

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncTuning as.data.table function works without resample result", {
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
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20, ncols = 15)
  expect_names(names(tab), permutation.of = c("cp", "minsplit", "classif.ce", "x_domain_cp", "x_domain_minsplit", "runtime_learners", "worker_id", "timestamp_xs", "timestamp_ys", "pid", "state", "keys", "warnings", "errors", "log"))

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncTuning as.data.table function works with empty archive", {
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
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncTuning as.data.table function works with new ids in x_domain", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

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

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    search_space = search_space
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20, ncols = 15)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncTuning as.data.table function works with switched new ids in x_domain", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

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

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE,
    search_space = search_space
  )
  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20, ncols = 15)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "keys", "warnings", "errors", "log"))

  expect_rush_reset(instance$rush)
})
