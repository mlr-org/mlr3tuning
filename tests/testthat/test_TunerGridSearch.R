context("TunerGridSearch")


test_that("TunerGridSearch",  {
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
  gs = TunerGridSearch$new(ff, terminator = terminator)

  result = gs$tune()$tune_result()
  exps = gs$ff$experiments
  expect_r6(gs, "TunerGridSearch")
  expect_data_table(exps, nrow = 2*5)
  expect_equal(exps[, uniqueN(hash)], 5)
  expect_equal(gs$settings$resolution, 5)
  result = gs$tune()$tune_result()
  expect_list(result)
  expect_number(result$performance, lower = measures$mmce$range[1], upper = measures$mmce$range[2])
  expect_list(result$param_vals, len = 2)
  expect_equal(result$param_vals$minsplit, 3)
})
