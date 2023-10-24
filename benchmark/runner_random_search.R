runner_random_search = function(renv_project, times) {

  # system("redis-server --daemonize yes --save \"\" --appendonly no")
  res = list()
  renv::load(renv_project)

  library(rush)
  library(mlr3tuning)
  library(mlr3tuningspaces)
  library(data.table)
  library(mlr3misc)
  library(microbenchmark)

  start_flush_redis = function() {
    future::plan("sequential")
    config = redux::redis_config()
    r = redux::hiredis(config)
    r$FLUSHDB()
    config
  }

  set.seed(1)
  options(width = 200)
  lgr::get_logger("rush")$set_threshold("warn")
  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  # random search with batch parallelization and chunk size of 1
  # this is the default
  # large overhead because a future is created for each resampling iteration
  options("mlr3.exec_chunk_size" = 1)

  instance_1 = ti(
    task = tsk("pima"),
    learner = lts(lrn("classif.rpart")),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1000),
    store_benchmark_result = FALSE,
    store_models = FALSE
  )

  tuner_1 = tnr("random_search", batch_size = 100)

  future::plan("multisession", workers = 4)

  res[["tune_1"]] = microbenchmark(
    tuning_1 = tuner_1$optimize(instance_1),
    times = times,
    setup = instance_1$clear(),
    unit = "s"
  )

  # random search with batch parallelization chunk size of 75
  # resampling iterations are evenly distributed among the workers
  # batch size of 100 allows to check terminators but adds overhead
  options("mlr3.exec_chunk_size" = 75)

  instance_2 = ti(
    task = tsk("pima"),
    learner = lts(lrn("classif.rpart")),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1000),
    store_benchmark_result = FALSE,
    store_models = FALSE
  )

  tuner_2 = tnr("random_search", batch_size = 100)

  future::plan("multisession", workers = 4)

  res[["tune_2"]] = microbenchmark(
    tuning_2 = tuner_2$optimize(instance_2),
    times = times,
    setup = instance_2$clear(),
    unit = "s"
  )

  # random search with batch parallelization chunk size of 750
  # resampling iterations are evenly distributed among the workers
  # batch size of 1000 minimizes overhead but does not allow to check terminators
  options("mlr3.exec_chunk_size" = 750)

  instance_3 = ti(
    task = tsk("pima"),
    learner = lts(lrn("classif.rpart")),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1000),
    store_benchmark_result = FALSE,
    store_models = FALSE
  )

  tuner_3 = tnr("random_search", batch_size = 1000)

  future::plan("multisession", workers = 4)

  res[["tune_3"]] = microbenchmark(
    tuning_3 = tuner_3$optimize(instance_3),
    times = times,
    setup = instance_3$clear(),
    unit = "s"
  )

  # random search with rush parallelization
  config = start_flush_redis()
  rush = rsh(config = config)

  instance_4 = ti(
    task = tsk("pima"),
    learner = lts(lrn("classif.rpart")),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1000),
    store_benchmark_result = FALSE,
    store_models = FALSE,
    rush = rush
  )

  setup = function() {
    rush$reset()
    future::plan("multisession", workers = 4)

  }

  tuner_4 = tnr("random_search")

  res[["tune_4"]] = microbenchmark(
    tuning_4 = {instance_4$start_workers(await_workers = TRUE); tuner_4$optimize(instance_4)},
    times = times,
    setup = setup(),
    unit = "s"
  )

  res
}
