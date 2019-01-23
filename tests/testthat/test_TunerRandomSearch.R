context("TunerRandomSearch")

test_that("TunerRandomSearch",  {
  n_folds = 4

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_vals = list(minsplit = 3)

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = n_folds)

  measures = mlr3::mlr_measures$mget(c("classif.mmce", "time_train", "time_both"))
  task$measures = measures

  param_set = paradox::ParamSet$new(params = list(
      paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(5)
  ff = FitnessFunction$new(task, learner, resampling, param_set)
  rs = TunerRandomSearch$new(ff, terminator)

  result = rs$tune()$tune_result()
  bmr = rs$ff$bmr
  expect_r6(rs, "TunerRandomSearch")
  expect_data_table(bmr$data, nrow = n_folds*5)
  expect_list(result)
  expect_number(result$performance["mmce"], lower = measures$classif.mmce$range[1], upper = measures$classif.mmce$range[2])
  expect_list(result$param_vals, len = 2)
  expect_equal(result$param_vals$minsplit, 3)
})
