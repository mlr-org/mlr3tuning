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
