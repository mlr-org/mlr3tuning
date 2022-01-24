test_that("tune function works", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))

  # single-crit
  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")

  # multi-crit
  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceMultiCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune function works without measure", {
  # single-crit
  instance = tune(method = "random_search", task = tsk("pima"),
    learner = lrn("classif.rpart", minsplit =  to_tune(1, 10)), resampling = rsmp ("holdout"), term_evals = 2)

  expect_measure(instance$objective$measures[[1]])
})
