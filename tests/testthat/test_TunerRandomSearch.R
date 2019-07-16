context("TunerRandomSearch")

test_that("TunerRandomSearch", {
  n_folds = 4

  task = mlr_tasks$get("iris")

  learner = mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)

  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = n_folds)

  measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(5)
  pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
  rs = TunerRandomSearch$new(pe, terminator)
  rs1 = TunerRandomSearch$new(pe, terminator, 2L)

  result = rs$tune()$tune_result()
  bmr = rs$pe$bmr
  expect_r6(rs, "TunerRandomSearch")
  expect_data_table(bmr$data, nrows = n_folds * 5)
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 2)
  expect_equal(result$values$minsplit, 3)
})

test_that("aggregate one row (#40)", {
  pe = PerformanceEvaluator$new(
    task = mlr_tasks$get("iris"),
    learner = mlr_learners$get("classif.rpart"),
    resampling = mlr_resamplings$get("cv"),
    measures = mlr_measures$get("classif.ce"),
    param_set = ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.01, upper = 0.1)
    ))
  )

  terminator_1 = TerminatorEvaluations$new(max_evaluations = 1)
  tuner_1 = TunerRandomSearch$new(pe, terminator = terminator_1, batch_size = 1)
  tuner_1$tune()
  tab = tuner_1$aggregate()
  expect_data_table(tab, nrows = 1)
  expect_number(tab$classif.ce)
})
