test_that("tune function works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$minsplit = to_tune(1, 10)

  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})
