context("tune_token")

test_that("tune_token does not clash with dependencies", {

  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("none")
  learner$param_set$add_dep("xval", "minsplit", CondEqual$new(3))

  learner$param_set$values$minsplit = to_tune(2, 3)
  learner$param_set$values$xval = to_tune(0, 1)

  search_space = learner$param_set$tune_ps()

  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = terminator
  )

  tuner = tnr("grid_search", resolution = 2)
  expect_error(tuner$optimize(instance), NA)
})
