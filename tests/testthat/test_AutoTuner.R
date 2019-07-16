context("AutoTuner")

test_that("AutoTuner / single step", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  measures = c("classif.ce", "time_train", "time_both")
  terminator = TerminatorEvaluations$new(3)

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))
  resampling = mlr_resamplings$get("cv3")

  at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerRandomSearch,
    tuner_settings = list(batch_size = 10L))
  at$store_bmr = TRUE

  expect_learner(at$train(task))
  expect_prediction(at$predict(task))
  expect_is(at$model, "rpart")
  expect_is(at$tuner, "Tuner")

  expect_benchmark_result(at$bmr)

  tab = at$tune_path
  expect_data_table(tab, nrows = 3)
  expect_subset(measures, names(tab))
  expect_subset("cp", names(tab))
})

test_that("AutoTuner", {
  outer_folds = 3L
  inner_folds = 2L
  inner_evals = 3L

  p_measures = c("classif.ce", "time_train", "time_both")

  task = mlr_tasks$get("iris")

  learner = mlr_learners$get("classif.rpart")

  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = inner_folds)

  measures = mlr_measures$mget(p_measures)

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(inner_evals)

  at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerRandomSearch,
    tuner_settings = list(batch_size = 10L))

  expect_null(at$bmr)

  # Nested Resampling:
  outer_resampling = mlr_resamplings$get("cv")
  outer_resampling$param_set$values = list(folds = outer_folds)
  rr = resample(task, at, outer_resampling)

  lapply(rr$learners, function(tuner) expect_r6(tuner, "AutoTuner"))

  at$train(task)
  expect_r6(at$tuner, "Tuner")

  # Nested Resampling:
  expect_data_table(rr$data, nrows = outer_folds)
  nuisance = lapply(rr$learners, function(autotuner) {
    expect_data_table(autotuner$tuner$pe$bmr$data, nrows = inner_evals * inner_folds)
    expect_data_table(autotuner$tuner$pe$bmr$aggregate(), nrows = inner_evals)
    expect_equal(names(autotuner$tuner$tune_result()$performance), p_measures)
    autotuner$tuner$tune_result()$performance
  })

  row_ids_inner = lapply(rr$data$learner, function(it) {
    it$tuner$pe$task$row_ids
  })
  row_ids_all = task$row_ids

  # Check if all sub tasks combined equals the full task:
  expect_set_equal(unlist(row_ids_inner), row_ids_all)

  # Check if each sub task of the inner tuner is a subset of the full task:
  nuisance = lapply(row_ids_inner, function(ids) {
    expect_true(any(!row_ids_all %in% ids))
  })


  # check that hyperpars are properly set for the returned learner
  set.seed(3)
  task = mlr_tasks$get("pima")

  at2 = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerRandomSearch,
    tuner_settings = list(batch_size = 10L))

  expect_null(at2$tuner)
  at2$train(task)

  # ensure that we have different scores
  expect_r6(at2$tuner, "Tuner")
  expect_equal(anyDuplicated(at2$tuner$aggregate()$classif.ce), 0L)

  expect_prediction(at2$predict(task))

  expect_equal(at2$tuner$aggregate()[which.min(classif.ce), cp], at2$param_set$values$cp)
  expect_equal(at2$data$learner$param_set$values, at2$tuner$tune_result()$values)
})
