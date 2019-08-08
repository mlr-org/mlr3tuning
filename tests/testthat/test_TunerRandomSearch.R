context("TunerRandomSearch")

test_that("TunerRandomSearch", {
  test_tuner(TunerRandomSearch)
})

test_that("aggregate one row (#40)", {
  pe = PerfEval$new(
    task = mlr_tasks$get("iris"),
    learner = mlr_learners$get("classif.rpart"),
    resampling = mlr_resamplings$get("cv"),
    measures = mlr_measures$get("classif.ce"),
    param_set = ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.01, upper = 0.1)
    ))
  )

  terminator_1 = TerminatorEvaluations$new(1)
  tuner_1 = TunerRandomSearch$new(pe, terminator = terminator_1, batch_size = 1)
  tuner_1$tune()
  tab = tuner_1$aggregate()
  expect_data_table(tab, nrows = 1)
  expect_number(tab$classif.ce)
})
