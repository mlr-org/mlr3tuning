test_that("tune function works with one measure", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(tuner = tnr("random_search", batch_size = 1), task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2)

  expect_class(instance, "TuningInstanceBatchSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune function works with multiple measures", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(tuner = tnr("random_search", batch_size = 1), task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")), term_evals = 2)

  expect_class(instance, "TuningInstanceBatchMultiCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune function works without measure", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(tuner = tnr("random_search"), task = tsk("pima"),
    learner = learner, resampling = rsmp ("holdout"), term_evals = 2)

  expect_measure(instance$objective$measures[[1]])
})

test_that("tune interface is equal to TuningInstanceBatchSingleCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args %nin% c("tuner", "method", "...", "rush")]
  tune_args[tune_args == "measures"] = "measure"

  instance_args = formalArgs(TuningInstanceBatchSingleCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})

test_that("tune interface is equal to TuningInstanceBatchMultiCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args %nin% c("tuner", "method", "...", "rush")]

  instance_args = formalArgs(TuningInstanceBatchMultiCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})

test_that("tune interface is equal to TuningInstanceAsyncSingleCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args %nin% c("tuner", "method", "...")]
  tune_args[tune_args == "measures"] = "measure"

  instance_args = formalArgs(TuningInstanceAsyncSingleCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})

test_that("tune interface is equal to TuningInstanceAsyncMultiCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args %nin% c("tuner", "method", "...")]

  instance_args = formalArgs(TuningInstanceAsyncMultiCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})
