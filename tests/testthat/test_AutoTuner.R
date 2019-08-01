context("AutoTuner")

test_that("AutoTuner / train+predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  measures = c("classif.ce", "time_train", "time_both")
  terminator = TerminatorEvaluations$new(3)

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))
  resampling = mlr_resamplings$get("holdout")

  at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerRandomSearch)
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

  # check tune result is correct
  expect_equal(at$tuner$aggregate()[which.min(classif.ce), cp], at$param_set$values$cp)
  expect_equal(at$data$learner$param_set$values, at$tuner$tune_result()$values)
})

test_that("AutoTuner / resample", {
  outer_folds = 2L
  inner_folds = 1L
  inner_evals = 3L

  p_measures = c("classif.ce", "time_train", "time_both")
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  r_inner = mlr_resamplings$get("holdout")
  r_outer = mlr_resamplings$get("cv", param_vals = list(folds = 2))
  measures = mlr_measures$mget(p_measures)

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(inner_evals)

  at = AutoTuner$new(learner, r_inner, measures, param_set, terminator, tuner = TunerRandomSearch)

  expect_null(at$bmr)

  rr = resample(task, at, r_outer)

  lapply(rr$learners, function(tuner) expect_r6(tuner, "AutoTuner"))

  # check tuning results of all outer folds
  expect_data_table(rr$data, nrows = outer_folds)
  lapply(rr$learners, function(autotuner) {
    expect_data_table(autotuner$tuner$pe$bmr$data, nrows = inner_evals * inner_folds)
    expect_data_table(autotuner$tuner$pe$bmr$aggregate(), nrows = inner_evals)
    expect_equal(names(autotuner$tuner$tune_result()$performance), p_measures)
    autotuner$tuner$tune_result()$performance
  })
})
