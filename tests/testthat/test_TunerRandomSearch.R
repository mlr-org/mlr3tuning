context("TunerRandomSearch")

test_that("TunerRandomSearch",  {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_vals = list(minsplit = 3)
  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 2)
  measures = mlr3::mlr_measures$mget("mmce")
  terminator = TerminatorEvaluations$new(5)
  param_set = paradox::ParamSet$new(params = list(
      paradox::ParamReal$new("cp", lower = 0.001, upper = 0.1
  )))

  ff = FitnessFunction$new(task, learner, resampling, measures, param_set)
  rs = TunerRandomSearch$new(ff, terminator)

  result = rs$tune()$tune_result()
  exps = rs$ff$experiments
  expect_r6(rs, "TunerRandomSearch")
  expect_data_table(exps, nrow = 2*5)
  expect_list(result)
  expect_number(result$performance, lower = measures$mmce$range[1], upper = measures$mmce$range[2])
  expect_list(result$param_vals, len = 2)
  expect_equal(result$param_vals$minsplit, 3)
})
