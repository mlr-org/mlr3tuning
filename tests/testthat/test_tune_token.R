context("tune_token")

test_that("tune_token does not clash with dependencies", {
  library(mlr3learners)

  task = tsk("iris")
  learner = lrn("classif.svm", type = "C-classification")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("none")

  learner$param_set$values$kernel = to_tune(c("polynomial", "radial"))
  learner$param_set$values$degree = to_tune(1, 3)

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
