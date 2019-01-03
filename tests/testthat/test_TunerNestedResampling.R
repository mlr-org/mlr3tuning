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

  expect_error(TunerNestedResampling$new(nested, outer))

  row_ids_inner = lapply(bmr$data$inner_tuner, function (it) {
    it$ff$task$row_ids[[1]]
  })
  row_ids_all = task$row_ids[[1]]
  
  expect_equal(sort(unique(unlist(row_ids_inner))), sort(row_ids_all))
  nuisance = lapply(row_ids_inner, function (ids) {
    expect_true(any(! row_ids_all %in% ids))
  })
})