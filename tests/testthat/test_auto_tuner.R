test_that("auto_tuner function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  at = auto_tuner(tuner = tnr("random_search", batch_size = 10), learner = learner, resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_evals = 50)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorEvals")

  at = auto_tuner(tuner = tnr("random_search", batch_size = 10), learner = learner, resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_time = 50)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorRunTime")

  at = auto_tuner(tuner = tnr("random_search", batch_size = 10), learner = learner, resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_evals = 10, term_time = 50)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorCombo")
})

test_that("async auto tuner works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  at = auto_tuner(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(at, "AutoTuner")
  at$train(tsk("pima"))

  expect_class(at$tuning_instance, "TuningInstanceAsyncSingleCrit")
})

test_that("async auto tuner works with rush controller", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  rush = rsh(network_id = "tuning_network")

  at = auto_tuner(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$rush, "Rush")
  at$train(tsk("pima"))

  expect_class(at$tuning_instance, "TuningInstanceAsyncSingleCrit")
})
