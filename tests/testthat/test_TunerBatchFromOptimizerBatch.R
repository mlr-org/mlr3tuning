test_that("TunerBatchFromOptimizerBatch parameter set works after cloning", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10)
  )
  tuner_1 = tnr("random_search", batch_size = 10)

  tuner_2 = tuner_1$clone(deep = TRUE)
  tuner_2$param_set$set_values(batch_size = 20)
  tuner_2$optimize(instance)

  expect_equal(nrow(instance$archive$data), 20L)
})
