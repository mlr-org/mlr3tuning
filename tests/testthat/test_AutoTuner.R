context("AutoTuner")

test_that("AutoTuner / train+predict", {
  measures = map(c("classif.ce", "time_train", "time_both"), msr)
  te = term("evals", n_evals = 3)
  task = tsk("iris")

  ps = TEST_MAKE_PS1()
  tuner = TunerRandomSearch$new()
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), measures, ps, te, tuner = tuner)
  at$store_bmr = TRUE

  at_clone = at$clone(deep = TRUE)
  expect_learner(at$train(task))
  expect_prediction({
    prd1 = at$predict(task)
  })
  at_clone$state = at$state
  expect_equal(at$clone(deep = TRUE), at_clone)
  expect_prediction({
    prd2 = at_clone$predict(task)
  })
  expect_equal(prd1, prd2)
  expect_is(at$model$learner$model, "rpart")

  expect_benchmark_result(at$model$bmr)
})

test_that("AutoTuner / resample", {
  outer_folds = 2L
  inner_folds = 1L
  inner_evals = 3L

  p_measures = c("classif.ce", "time_train", "time_both")
  measures = mlr_measures$mget(p_measures)
  r_inner = rsmp("holdout")
  r_outer = rsmp("cv", folds = 2)

  param_set = TEST_MAKE_PS1()

  te = term("evals", n_evals = inner_evals)

  tuner = TunerRandomSearch$new()
  at = AutoTuner$new(lrn("classif.rpart"), r_inner, measures, param_set, te, tuner)

  expect_null(at$model$bmr)

  rr = resample(tsk("iris"), at, r_outer)


  # check tuning results of all outer folds
  expect_data_table(rr$data, nrows = outer_folds)
  # lapply(rr$learners, function(autotuner) {
  #   expect_data_table(autotuner$tuner$pe$bmr$data, nrows = inner_evals * inner_folds)
  #   expect_data_table(autotuner$tuner$pe$bmr$archive(), nrows = inner_evals)
  #   expect_equal(names(autotuner$tuner$tune_result()$score), p_measures)
  #   autotuner$tuner$tune_result()$score
  # })
})

# we had an issue that the AutoTuner did not return statically configured param in its result
# see issue #51
test_that("AutoTuner / param_set", {
  measures = msr("classif.ce")
  te = term("evals", n_evals = 3)
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  tuner = TunerRandomSearch$new()
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), measures, ps, te, tuner)
  at$param_set$values$maxdepth = 1
  at$param_set$values$cp = 1
  expect_equal(at$param_set$values[names(at$learner$param_set$values)], at$learner$param_set$values)
  at$train(task)

  # parameter that is not in training ps was used
  expect_equal(at$model$learner$model$control$maxdepth, 1)
  # parameter that *is* in training ps was changed (to inside training range)
  expect_lt(at$model$learner$model$control$cp, ps$params$cp$upper)

  expect_equal(at$param_set$values$maxdepth, 1)
  expect_equal(at$param_set$values$cp, 1)

  # param set, including id, survives clone
  at$param_set$set_id = "xyz"
  at2 = at$clone(deep = TRUE)
  expect_equal(at$param_set, at2$param_set)
})
