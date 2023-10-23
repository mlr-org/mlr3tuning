test_that("initializing TuningInstanceRushSingleCrit works", {
  rush = rsh()

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_r6(instance$archive, "ArchiveRushTuning")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$terminator, "Terminator")
  expect_r6(instance$rush, "Rush")
  expect_null(instance$result)

  rush$reset()
})

test_that("starting workers with OptimInstanceRushSingleCrit works", {
  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  expect_equal(rush$n_workers, 1L)

  rush$reset()
})

test_that("evaluating points works with OptimInstanceRushSingleCrit", {
  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  xdt = data.table(cp = c(0.3, 0.25), minsplit = c(3, 4))
  keys = instance$eval_async(xdt)
  rush$await_tasks(keys)

  expect_data_table(instance$archive$data, nrows = 2L)

  rush$reset()
})

test_that("assigning a result works with OptimInstanceRushSingleCrit", {
  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)
  xdt = data.table(cp = c(0.3, 0.25), minsplit = c(3, 4))
  keys = instance$eval_async(xdt)
  rush$await_tasks(keys)

  expect_null(instance$result)
  get_private(instance)$.assign_result(xdt = xdt[2, ], y = c(classif.ce = 0.2))
  expect_equal(instance$result,
    cbind(xdt[2, ],
    learner_param_vals = list(list(xval = 0, cp = 0.25, minsplit = 4)),
    x_domain = list(list(cp = 0.25, minsplit = 4)),
    classif.ce = 0.2))

  rush$reset()
})


test_that("OptimInstanceRushSingleCrit and the cluster backend works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)
  expect_data_table(as.data.table(instance$archive), nrows = 3)

  rush$reset()
})

test_that("OptimInstanceRushSingleCrit and the multisession backend works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  future::plan("multisession", workers = 2L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)
  expect_data_table(as.data.table(instance$archive), nrows = 3)

  rush$reset()
})

test_that("saving the benchmark result with OptimInstanceRushSingleCrit works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_equal(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)

  rush$reset()
})

test_that("saving the models with OptimInstanceRushSingleCrit works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_equal(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_class(instance$archive$resample_result(1)$learners[[1]]$model, "rpart")

  rush$reset()
})

test_that("freezing ArchiveRush after the optimization works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE, freeze_archive = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  expect_null(instance$archive$rush)
  expect_data_table(as.data.table(instance$archive), nrows = 3L)
  expect_benchmark_result(instance$archive$benchmark_result)

  rush$reset()
})

test_that("log messages are saved", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE, lgr_thresholds = c(rush = "debug", mlr3 = "info"))
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)
  tuner = tnr("random_search")
  tuner$optimize(instance)

  log = rush$read_log()
  expect_data_table(log, nrows = 22)
  expect_names(names(log), must.include = c("worker_id", "timestamp", "logger", "msg"))
  expect_subset(log$logger, c("rush", "mlr3"))

  rush$reset()
})

# fault detection --------------------------------------------------------------

test_that("segfault on rush worker", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  learner = lrn("classif.debug", segfault_train = 1, x = to_tune())

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
  )

  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE, detect_lost_tasks = TRUE)
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)
  tuner = tnr("random_search")

  expect_error(tuner$optimize(instance), "Can't assign result to")

  rush$reset()
})
