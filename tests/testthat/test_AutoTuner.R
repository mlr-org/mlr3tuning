context("AutoTuner")

test_that("AutoTuner / train+predict", {
  measures = c("classif.ce", "time_train", "time_both")
  terminator = TerminatorEvaluations$new(3)
  task = "iris"

  ps = TEST_MAKE_PS1()
  at = AutoTuner$new("classif.rpart", "holdout", measures, ps, terminator, tuner = TunerRandomSearch)
  at$store_bmr = TRUE

  expect_learner(at$train(task))
  expect_prediction(at$predict(task))
  expect_is(at$model, "rpart")
  expect_is(at$tuner, "Tuner")

  expect_benchmark_result(at$bmr)

  tab = at$archive()
  expect_data_table(tab, nrows = 3)
  expect_subset(measures, names(tab))
  expect_subset("cp", names(tab))

  # check tune result is correct
  expect_equal(at$tuner$archive()[which.min(classif.ce), cp], at$param_set$values$cp)
  expect_equal(at$learner$param_set$values, at$tuner$tune_result()$values)
})

test_that("AutoTuner / resample", {
  outer_folds = 2L
  inner_folds = 1L
  inner_evals = 3L

  p_measures = c("classif.ce", "time_train", "time_both")
  r_inner = mlr_resamplings$get("holdout")
  r_outer = mlr_resamplings$get("cv", param_vals = list(folds = 2))
  measures = mlr_measures$mget(p_measures)

  param_set = TEST_MAKE_PS1()

  terminator = TerminatorEvaluations$new(inner_evals)

  at = AutoTuner$new("classif.rpart", r_inner, measures, param_set, terminator, tuner = TunerRandomSearch)

  expect_null(at$bmr)

  rr = resample("iris", at, r_outer)

  lapply(rr$learners, function(tuner) expect_r6(tuner, "AutoTuner"))

  # check tuning results of all outer folds
  expect_data_table(rr$data, nrows = outer_folds)
  # lapply(rr$learners, function(autotuner) {
  #   expect_data_table(autotuner$tuner$pe$bmr$data, nrows = inner_evals * inner_folds)
  #   expect_data_table(autotuner$tuner$pe$bmr$archive(), nrows = inner_evals)
  #   expect_equal(names(autotuner$tuner$tune_result()$performance), p_measures)
  #   autotuner$tuner$tune_result()$performance
  # })
})
