context("AutoTuner")

test_that("AutoTuner / train+predict", {
  te = term("evals", n_evals = 3)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = MeasureDummyCPClassif$new(fun = function(cp) if (cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te, tuner = tuner)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  expect_equal(at$learner$param_set$values, list(xval = 0, cp = 0.2))
  inst = at$tuning_instance
  expect_benchmark_result(inst$bmr)
  a = at$archive()
  expect_data_table(a, nrows = 3L)
  r = at$tuning_result
  expect_equal(r$tune_x, list(cp = 0.2))
  prd = at$predict(task)
  expect_prediction(prd)
  expect_is(at$learner$model, "rpart")
})

test_that("AutoTuner / resample", {
  outer_folds = 2L
  inner_folds = 1L
  inner_evals = 3L

  ms = MeasureDummyCPClassif$new(fun = function(cp) if (cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
  tuner = tnr("grid_search", resolution = 3)
  r_inner = rsmp("holdout")
  r_outer = rsmp("cv", folds = 2)
  param_set = TEST_MAKE_PS1()
  te = term("evals", n_evals = inner_evals)
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(lrn("classif.rpart"), r_inner, ms, param_set, te, tuner)

  expect_null(at$tuning_instance)

  rr = resample(tsk("iris"), at, r_outer, store_models = TRUE)

  # check tuning results of all outer folds
  expect_data_table(rr$data, nrows = outer_folds)
  lapply(rr$learners, function(ll) {
    assert_r6(ll, "AutoTuner")
    expect_equal(ll$learner$param_set$values, list(xval = 0, cp = 0.2))
    inst = ll$tuning_instance
    assert_r6(inst, "TuningInstance")
    r = inst$result
    expect_data_table(inst$bmr$data, nrows = inner_evals * inner_folds)
    expect_data_table(inst$archive(), nrows = inner_evals)
    expect_numeric(r$perf, len = 1L)
  })
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
  expect_equal(at$learner$model$control$maxdepth, 1)
  # parameter that *is* in training ps was changed (to inside training range)
  expect_lt(at$learner$model$control$cp, ps$params$cp$upper)

  expect_equal(at$param_set$values$maxdepth, 1)
  # expect_equal(at$param_set$values$cp, 1)

  # param set, including id, survives clone
  at$param_set$set_id = "xyz"
  at2 = at$clone(deep = TRUE)
  expect_equal(at$param_set, at2$param_set)
})



