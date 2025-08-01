# stages in $optimize() --------------------------------------------------------

test_that("on_optimization_begin works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_optimization_begin = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$terminator$param_set$values$n_evals, 20)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_optimization_end works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_optimization_end = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$terminator$param_set$values$n_evals, 20)
  expect_rush_reset(instance$rush, type = "kill")
})

# stager in worker_loop() ------------------------------------------------------

test_that("on_worker_begin works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_worker_begin = function(callback, context) {
      instance = context$instance
      mlr3misc::get_private(instance)$.eval_point(list(minsplit = 1))
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_subset(1, instance$archive$data$minsplit)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_worker_end works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_worker_end = function(callback, context) {
      instance = context$instance
      mlr3misc::get_private(instance)$.eval_point(list(minsplit = 1))
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_subset(1, instance$archive$data$minsplit)
  expect_rush_reset(instance$rush, type = "kill")
})

# stages in $.eval_point() -----------------------------------------------------

test_that("on_optimizer_before_eval and on_optimizer_after_eval works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_optimizer_before_eval = function(callback, context) {
      context$xs = list(minsplit = 1)
      context$xs_trafoed = list(minsplit = 0)
    },

    on_optimizer_after_eval = function(callback, context) {
      context$ys = list(classif.ce = 0)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_equal(unique(instance$archive$data$minsplit), 1)
  expect_equal(unique(instance$archive$data$classif.ce), 0)
  expect_rush_reset(instance$rush, type = "kill")
})

# stages in $eval() ------------------------------------------------------------

test_that("on_eval_after_xs works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_eval_after_xs = function(callback, context) {
      context$xs_learner$minsplit = 1
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_equal(instance$archive$benchmark_result$resample_result(1)$learner$param_set$values$minsplit, 1)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_eval_after_resample works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_eval_after_resample = function(callback, context) {
      callback$state$extra_performance = context$resample_result$aggregate(msr("classif.acc"))
    },

    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance$classif.acc = callback$state$extra_performance
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_names(names(instance$archive$data), must.include = c("classif.ce", "classif.acc"))
  expect_rush_reset(instance$rush, type = "kill")
})

# stages in $assign_result() in TuningInstanceAsyncSingleCrit ------------------

test_that("on_tuning_result_begin in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_y = c(classif.ce = 0.7)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")
  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$result$minsplit, 1)
  expect_equal(instance$result$classif.ce, 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_result_end in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  callback = callback_async_tuning(id = "test",
    on_result_end = function(callback, context) {
      context$result$classif.ce = 0.7
    }
  )

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$result$classif.ce, 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_result in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  expect_warning({callback = callback_async_tuning(id = "test",
    on_result = function(callback, context) {
      context$result$classif.ce = 0.7
    }
  )}, "deprecated")

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$result$classif.ce, 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

# stages in $assign_result() in TuningInstanceBatchMultiCrit -------------------

test_that("on_tuning_result_begin in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_ydt = data.table(classif.ce = 0.7, classif.acc = 0.8)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(instance$result$minsplit, 1)
  expect_equal(instance$result$classif.ce, 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_result_end in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})


  callback = callback_async_tuning(id = "test",
    on_result_end = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(unique(instance$result$classif.ce), 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_result in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})


  expect_warning({callback = callback_async_tuning(id = "test",
    on_result = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )}, "deprecated")

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")
  expect_equal(unique(instance$result$classif.ce), 0.7)
  expect_rush_reset(instance$rush, type = "kill")
})

# stages in mlr3 workhorse -----------------------------------------------------

test_that("on_resample_begin works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning("test",
    on_resample_begin = function(callback, context) {
      # expect_* does not work
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      checkmate::assert_number(context$iteration)
      checkmate::assert_null(context$pdatas)
      context$data_extra = list(success = TRUE)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")

  walk(as.data.table(instance$archive$benchmark_result)$data_extra, function(data_extra) {
    expect_true(data_extra$success)
  })
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_resample_before_train works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning("test",
    on_resample_before_train = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      checkmate::assert_number(context$iteration)
      checkmate::assert_null(context$pdatas)
      context$data_extra = list(success = TRUE)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")

  walk(as.data.table(instance$archive$benchmark_result)$data_extra, function(data_extra) {
    expect_true(data_extra$success)
  })
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_resample_before_predict works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning("test",
    on_resample_before_predict = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      checkmate::assert_null(context$pdatas)
      context$data_extra = list(success = TRUE)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")

  walk(as.data.table(instance$archive$benchmark_result)$data_extra, function(data_extra) {
    expect_true(data_extra$success)
  })
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("on_resample_end works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  callback = callback_async_tuning("test",
    on_resample_end = function(callback, context) {
      # expect_* does not work
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      checkmate::assert_number(context$iteration)
      checkmate::assert_class(context$pdatas$test, "PredictionData")
      context$learner$state = mlr3misc::insert_named(context$learner$state, list(state_success = TRUE))
      context$data_extra = list(success = TRUE)
    }
  )

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextAsyncTuning")

  walk(as.data.table(instance$archive$benchmark_result)$data_extra, function(data_extra) {
    expect_true(data_extra$success)
  })

  walk(instance$archive$benchmark_result$score()$learner, function(learner, ...) {
    expect_true(learner$state$state_success)
  })
  expect_rush_reset(instance$rush, type = "kill")
})
