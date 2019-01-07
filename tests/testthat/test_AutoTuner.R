context("AutoTuner")

test_that("AutoTuner",  {
  outer_folds = 3L
  inner_folds = 4L
  inner_evals = 5L

  p_measures = c("mmce", "time_train", "time_both")

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = inner_folds)

  measures = mlr3::mlr_measures$mget(p_measures)
  task$measures = measures

  param_set = paradox::ParamSet$new(params = list(
    paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(inner_evals)

  at = AutoTuner$new(learner, resampling, param_set, terminator, tuner = TunerRandomSearch, 
    tuner_settings = list(batch_size = 10L))  

  # Nested Resampling:
  outer_resampling = mlr3::mlr_resamplings$get("cv")
  outer_resampling$param_vals = list(folds = outer_folds)
  browser()
  r = mlr3::resample(task, at, outer_resampling)
  
  # Nested Resampling:
  checkmate::expect_data_table(r$data, nrow = outer_folds)
  nuisance = lapply(r$data$learner, function (autotuner) {
    checkmate::expect_data_table(autotuner$tuner$ff$bmr$data, nrow = inner_evals * inner_folds)
    checkmate::expect_data_table(autotuner$tuner$ff$bmr$aggregated, nrow = inner_evals)
    expect_equal(names(autotuner$tuner$tune_result()$performance), p_measures)
  })

  row_ids_inner = lapply(r$data$learner, function (it) {
    it$tuner$ff$task$row_ids[[1]]
  })
  row_ids_all = task$row_ids[[1]]
  
  expect_equal(sort(unique(unlist(row_ids_inner))), sort(row_ids_all))
  nuisance = lapply(row_ids_inner, function (ids) {
    expect_true(any(! row_ids_all %in% ids))
  })


  at2 = AutoTuner$new(learner, resampling, param_set, terminator, tuner = TunerRandomSearch, 
    tuner_settings = list(batch_size = 10L))

  expect_null(at2$tuner)

  at2$train(task)

  checkmate::expect_r6(at2$tuner, "Tuner")
  checkmate::expect_r6(at2$predict(task), "Prediction")

  expect_equal(at2$learner$param_vals, at2$tuner$tune_result()$param_vals)
})