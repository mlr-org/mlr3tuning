context("TunerNestedResampling")

test_that("TunerNestedResampling",  {
  outer_folds = 3L
  inner_folds = 4L
  inner_evals = 5L

  p_measures = c("mmce", "time_train", "time_both")

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_vals = list(minsplit = 3)

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = inner_folds)

  measures = mlr3::mlr_measures$mget(p_measures)
  task$measures = measures

  param_set = paradox::ParamSet$new(params = list(
      paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(inner_evals)
  ff = FitnessFunction$new(task, learner, resampling, param_set)
  inner_tuner = TunerRandomSearch$new(ff, terminator)
  outer = mlr3::mlr_resamplings$get("cv")
  outer$param_vals = list(folds = outer_folds)
  nested = TunerNestedResampling$new(inner_tuner, outer)

  expect_error(nested$performance())

  nested$tune()
  p = nested$performance()
  bmr = nested$ff$bmr

  expect_r6(nested, "TunerNestedResampling")
  expect_data_table(bmr$data, nrow = outer_folds)
  lapply(bmr$data$inner_tuner, function (tuner) {
    expect_data_table(tuner$ff$bmr$data, nrow = inner_evals * inner_folds)
    expect_data_table(tuner$ff$bmr$aggregated, nrow = inner_evals)
  })
  expect_equal(names(p), p_measures)
  # expect_list(result)
  # expect_number(result$performance, lower = measures$mmce$range[1], upper = measures$mmce$range[2])
  # expect_list(result$param_vals, len = 2)
  # expect_equal(result$param_vals$minsplit, 3)
})