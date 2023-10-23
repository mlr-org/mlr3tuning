test_that("ArchiveRushTuning access methods work", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  # benchmark result
  expect_benchmark_result(instance$archive$benchmark_result)
  expect_equal(instance$archive$benchmark_result$n_resample_results, 4L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)

  # learner
  walk(seq(rush$n_finished_tasks), function(i) {
    expect_learner(instance$archive$learner(i = i))
  })

  # learner param values
  walk(seq(rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_named(instance$archive$learner_param_vals(i), c("xval" ,"cp"))
  })

  # learners
  walk(seq(rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  walk(seq(rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  walk(seq(rush$n_finished_tasks), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

})

test_that("ArchiveRushTuning as.data.table function works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 11)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4, ncols = 12)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys", "classif.acc"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4, ncols = 13)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys", "classif.acc", "classif.mcc"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp_xs")
  expect_data_table(tab, nrows = 4, ncols = 10)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_ys", "pid", "state", "keys"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp_xs", "resample_result"))
  expect_data_table(tab, nrows = 4, ncols = 9)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "timestamp_ys", "pid", "state", "keys"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4, ncols = 11)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4, ncols = 11)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "x_domain", "state", "keys"))
})

test_that("ArchiveRushTuning as.data.table function works without resample result", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    store_benchmark_result = FALSE,
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 10)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "worker_id", "timestamp_xs", "timestamp_ys", "pid", "state", "keys"))
})

test_that("ArchiveRushTuning as.data.table function works with empty archive", {
  rush = rsh()
  future::plan("cluster", workers = 1L)

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    store_benchmark_result = FALSE,
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)

  rush$reset()
})

test_that("ArchiveRushTuning as.data.table function works with new ids in x_domain", {
  skip_on_cran()
  skip_on_ci()

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

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    search_space,
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 13)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys"))

  rush$reset()
})

test_that("ArchiveRushTuning as.data.table function works with switched new ids in x_domain", {
  skip_on_cran()
  skip_on_ci()

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

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 100),
    search_space,
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 100, ncols = 13)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "worker_id", "resample_result", "timestamp_xs", "timestamp_ys", "pid", "state", "keys"))

  rush$reset()
})
