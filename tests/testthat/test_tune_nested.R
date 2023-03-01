test_that("tune_nested function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  rr = tune_nested(tuner = tnr("random_search", batch_size = 1), task = tsk("pima"), learner = learner, inner_resampling = rsmp ("holdout"),
    outer_resampling = rsmp("cv", folds = 3), measure = msr("classif.ce"), term_evals = 2)

  expect_resample_result(rr)
  expect_equal(rr$resampling$id, "cv")
  expect_equal(rr$resampling$iters, 3)
  expect_data_table(extract_inner_tuning_results(rr), nrows = 3)
  expect_class(rr$learners[[1]], "AutoTuner")
  expect_equal(rr$learners[[1]]$tuning_instance$objective$resampling$id, "holdout")
})
