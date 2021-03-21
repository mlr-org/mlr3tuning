test_that("auto_tuner function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  at = auto_tuner(method = "random_search", learner = learner, resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 50, batch_size = 10)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorEvals")

  at = auto_tuner(method = "random_search", learner = learner, resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_time = 50, batch_size = 10)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorRunTime")

  at = auto_tuner(method = "random_search", learner = learner, resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 10, term_time = 50, batch_size = 10)

  expect_class(at, "AutoTuner")
  expect_class(at$instance_args$terminator, "TerminatorCombo")
})

test_that("tune function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune_nested function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  rr = tune_nested(method = "random_search", task = tsk("pima"), learner = learner, inner_resampling = rsmp ("holdout"), 
    outer_resampling = rsmp("cv", folds = 3), measure = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_resample_result(rr)
  expect_equal(rr$resampling$id, "cv")
  expect_equal(rr$resampling$iters, 3)
  expect_data_table(extract_inner_tuning_results(rr), nrows = 3)
  expect_class(rr$learners[[1]], "AutoTuner")
  expect_equal(rr$learners[[1]]$tuning_instance$objective$resampling$id, "holdout")
})
