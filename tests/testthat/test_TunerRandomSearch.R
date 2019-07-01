context("TunerRandomSearch")

test_that("TunerRandomSearch", {
  n_folds = 4

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = n_folds)

  measures = mlr3::mlr_measures$mget(c("classif.ce", "time_train", "time_both"))

  param_set = paradox::ParamSet$new(params = list(
    paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(5)
  pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
  rs = TunerRandomSearch$new(pe, terminator)
  rs1 = TunerRandomSearch$new(pe, terminator, 2L)

  result = rs$tune()$tune_result()
  bmr = rs$pe$bmr
  expect_r6(rs, "TunerRandomSearch")
  expect_data_table(bmr$data, nrow = n_folds * 5)
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 2)
  expect_equal(result$values$minsplit, 3)
})
