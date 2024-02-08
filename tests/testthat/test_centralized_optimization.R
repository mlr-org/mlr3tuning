test_that("Random Search works with OptimInstanceRushSingleCrit", {
  rush_plan(n_workers = 2)

  learner = lrn("classif.rpart",
      minsplit  = to_tune(2, 128),
      cp        = to_tune(1e-04, 1e-1))

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  optimizer = tnr("random_search")
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 2L)

  expect_rush_reset(instance$rush)
})

test_that("random search with transformation functions work", {
  rush_plan(n_workers = 2)

  learner = lrn("classif.rpart",
      minsplit  = to_tune(2, 128, logscale = TRUE),
      cp        = to_tune(1e-04, 1e-1, logscale = TRUE))

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  optimizer = tnr("random_search")
  optimizer$optimize(instance)

  instance$archive$data

  expect_data_table(instance$archive$data, min.rows = 2L)

  expect_rush_reset(instance$rush)
})

