# stages in $optimize() --------------------------------------------------------

test_that("on_optimization_begin works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_optimization_begin = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
    }
  )

  rush::rush_plan(n_workers = 2)
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
})

test_that("on_optimization_end works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_optimization_end = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
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
  expect_equal(instance$terminator$param_set$values$n_evals, 20)
})

# stager in worker_loop() ------------------------------------------------------

test_that("on_worker_begin works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_worker_begin = function(callback, context) {
      instance = context$instance
      mlr3misc::get_private(instance)$.eval_point(list(minsplit = 1))
    }
  )

  rush::rush_plan(n_workers = 2)
  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_subset(1, instance$archive$data$minsplit)
})

test_that("on_worker_end works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_worker_end = function(callback, context) {
      instance = context$instance
      mlr3misc::get_private(instance)$.eval_point(list(minsplit = 1))
    }
  )

  rush::rush_plan(n_workers = 2)
  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_subset(1, instance$archive$data$minsplit)
})

# stages in $.eval_point() -----------------------------------------------------

test_that("on_optimizer_before_eval and on_optimizer_after_eval works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_optimizer_before_eval = function(callback, context) {
      context$xs = list(minsplit = 1)
      context$xs_trafoed = list(minsplit = 0)
    },

    on_optimizer_after_eval = function(callback, context) {
      context$ys = list(classif.ce = 0)
    }
  )

  rush::rush_plan(n_workers = 2)
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
})

# stages in $eval() ------------------------------------------------------------

test_that("on_eval_after_xs works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_eval_after_xs = function(callback, context) {
      context$xs_learner$minsplit = 1
    }
  )
  rush::rush_plan(n_workers = 2)
  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_equal(instance$archive$benchmark_result$resample_result(1)$learner$param_set$values$minsplit, 1)
})

test_that("on_eval_after_resample works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_eval_after_resample = function(callback, context) {
      callback$state$extra_performance = context$resample_result$aggregate(msr("classif.acc"))
    },

    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance$classif.acc = callback$state$extra_performance
    }
  )

  rush::rush_plan(n_workers = 2)
  instance = tune(
    tuner = tnr("async_random_search"),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_names(names(instance$archive$data), must.include = c("classif.ce", "classif.acc"))
})

# stages in $assign_result() in TuningInstanceAsyncSingleCrit ------------------

test_that("on_tuning_result_begin in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_y = c(classif.ce = 0.7)
    }
  )

  rush::rush_plan(n_workers = 2)
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
})

test_that("on_result_end in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
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
})

test_that("on_result in TuningInstanceSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  expect_warning({callback = callback_async_tuning(id = "test",
    on_result = function(callback, context) {
      context$result$classif.ce = 0.7
    }
  )}, "deprecated")

  rush::rush_plan(n_workers = 2)
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
})

# stages in $assign_result() in TuningInstanceBatchMultiCrit -------------------

test_that("on_tuning_result_begin in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  callback = callback_async_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_ydt = data.table(classif.ce = 0.7, classif.acc = 0.8)
    }
  )

  rush::rush_plan(n_workers = 2)
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
})

test_that("on_result_end in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()


  callback = callback_async_tuning(id = "test",
    on_result_end = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )

  rush::rush_plan(n_workers = 2)
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
})

test_that("on_result in TuningInstanceBatchMultiCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()


  expect_warning({callback = callback_async_tuning(id = "test",
    on_result = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )}, "deprecated")

  rush::rush_plan(n_workers = 2)
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
})

