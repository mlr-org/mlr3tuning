# stages in $optimize() --------------------------------------------------------

test_that("on_optimization_begin works", {
  callback = callback_batch_tuning(id = "test",
    on_optimization_begin = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$terminator$param_set$values$n_evals, 20)
})

test_that("on_optimization_end works", {
  callback = callback_batch_tuning(id = "test",
    on_optimization_end = function(callback, context) {
      context$instance$terminator$param_set$values$n_evals = 20
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$terminator$param_set$values$n_evals, 20)
})

# stages in $eval_batch() ------------------------------------------------------

test_that("on_optimizer_after_eval works", {
  callback = callback_batch_tuning(id = "test",
    on_optimizer_before_eval = function(callback, context) {
      set(context$xdt, j = "minsplit", value = 1)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)


  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(unique(instance$archive$data$minsplit), 1)
})

test_that("on_optimizer_after_eval works", {
  callback = callback_batch_tuning(id = "test",
    on_optimizer_after_eval = function(callback, context) {
      set(context$instance$archive$data, j = "classif.ce", value = 0.5)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)


  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(unique(instance$archive$data$classif.ce), 0.5)
})

# stages in $eval_many() -------------------------------------------------------

test_that("on_eval_after_design works", {
  callback = callback_batch_tuning(id = "test",
    on_eval_after_design = function(callback, context) {
      context$design$param_values[[1]][[1]] = list(list(minsplit = 1))
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$archive$benchmark_result$resample_result(1)$learner$param_set$values$minsplit, 1)
})

test_that("on_eval_after_benchmark and on_eval_before_archive works", {
  callback = callback_batch_tuning(id = "test",
    on_eval_after_benchmark = function(callback, context) {
      callback$state$extra_performance = context$benchmark_result$aggregate(msr("classif.acc"))[, "classif.acc", with = FALSE]
    },

    on_eval_before_archive = function(callback, context) {
      set(context$aggregated_performance, j = "classif.acc", value = callback$state$extra_performance)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_names(names(instance$archive$data), must.include = c("classif.ce", "classif.acc"))
})

# stages in $assign_result() in TuningInstanceBatchSingleCrit ------------------

test_that("on_tuning_result_begin in TuningInstanceSingleCrit works", {
  callback = callback_batch_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_y = c(classif.ce = 0.7)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$result$minsplit, 1)
  expect_equal(instance$result$classif.ce, 0.7)
})

test_that("on_result_end in TuningInstanceSingleCrit works", {
  callback = callback_batch_tuning(id = "test",
    on_result_end = function(callback, context) {
      context$result$classif.ce = 0.7
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$result$classif.ce, 0.7)
})

test_that("on_result in TuningInstanceSingleCrit works", {
  expect_warning({callback = callback_batch_tuning(id = "test",
    on_result = function(callback, context) {
      context$result$classif.ce = 0.7
    }
  )}, "deprecated")

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$result$classif.ce, 0.7)
})

# stages in $assign_result() in TuningInstanceBatchMultiCrit -------------------

test_that("on_tuning_result_begin in TuningInstanceBatchMultiCrit works", {
  callback = callback_batch_tuning(id = "test",
    on_tuning_result_begin = function(callback, context) {
      context$result_xdt = data.table(minsplit = 1)
      context$result_ydt = data.table(classif.ce = 0.7, classif.acc = 0.8)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(instance$result$minsplit, 1)
  expect_equal(instance$result$classif.ce, 0.7)
})

test_that("on_result_end in TuningInstanceBatchMultiCrit works", {
  expect_warning({callback = callback_batch_tuning(id = "test",
    on_result = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )}, "deprecated")

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(unique(instance$result$classif.ce), 0.7)
})

test_that("on_result in TuningInstanceBatchMultiCrit works", {
  callback = callback_batch_tuning(id = "test",
    on_result_end = function(callback, context) {
      set(context$result, j = "classif.ce", value = 0.7)
    }
  )

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit = to_tune(1, 10)),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 2,
    callbacks = callback)

  expect_class(instance$objective$context, "ContextBatchTuning")
  expect_equal(unique(instance$result$classif.ce), 0.7)
})


