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
  a = at$archive$data()
  expect_data_table(a, nrows = 3L)
  r = at$tuning_result
  expect_equal(r$x_domain[[1]], list(cp = 0.2))
  expect_equal(r$learner_param_vals[[1]], list(xval = 0, cp = 0.2))
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
  at = AutoTuner$new(lrn("classif.rpart", predict_type = "prob"), r_inner, ms, param_set, te, tuner)

  expect_null(at$tuning_instance)
  expect_equal(at$predict_type, "prob")

  rr = resample(tsk("iris"), at, r_outer, store_models = TRUE)

  # check tuning results of all outer folds
  expect_data_table(rr$data, nrows = outer_folds)
  lapply(rr$learners, function(ll) {
    assert_r6(ll, "AutoTuner")
    expect_equal(ll$learner$param_set$values, list(xval = 0, cp = 0.2))
    inst = ll$tuning_instance
    assert_r6(inst, "TuningInstance")
    expect_data_table(inst$archive$data(), nrows = inner_evals)
    expect_numeric(inst$result_y, len = 1L)
  })
})

# we had an issue that the AutoTuner did not return statically configured param in its result
# see issue #51
test_that("AutoTuner / param_set", {
  measure = msr("classif.ce")
  te = term("evals", n_evals = 3)
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  tuner = TunerRandomSearch$new()
  learner = lrn("classif.rpart", cp = 1, maxdepth = 1)
  at = AutoTuner$new(learner, rsmp("holdout"), measure, ps, te, tuner)
  expect_equal(at$param_set$values[names(at$learner$param_set$values)], at$learner$param_set$values)
  at$train(task)

  # parameter that is not in training ps was used
  expect_equal(at$param_set$values$maxdepth, 1)
  expect_equal(at$learner$param_set$values$maxdepth, 1)
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


test_that("Custom resampling is not allowed", {
  measure = msr("classif.ce")
  te = term("evals", n_evals = 3)
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  tuner = TunerRandomSearch$new()
  r = rsmp("holdout")$instantiate(task)
  expect_error(AutoTuner$new(lrn("classif.rpart"), r, measure, ps, te, tuner), "instantiated")
})



test_that("nested resamppling results are consistent ", {
  # we had a bad pointer bug due to missing cloning here
  # https://github.com/mlr-org/mlr3/issues/428
  # this resulted in different tuning results stored in models than used in final training

  ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)))


  lrn = AutoTuner$new(
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    search_space = ps,
    measure = msr("classif.ce"),
    terminator = term("evals", n_evals = 3),
    tuner = tnr("random_search")
  )

  cv2 = rsmp("cv", folds = 2)
  rr = resample(tsk("iris"), lrn, cv2, store_models = TRUE)
  ll1 = rr$learners[[1]]
  ll2 = rr$learners[[2]]
  tr1 = ll1$tuning_result
  tr2 = ll2$tuning_result
  expect_equal(tr1$x_domain[[1]], ll1$model$learner$model$control[c("cp", "minsplit")])
  expect_equal(tr2$x_domain[[1]], ll2$model$learner$model$control[c("cp", "minsplit")])
})

test_that("AT training does not change learner in instance args", {
  # we had a bad pointer bug due to missing cloning here
  #https://github.com/mlr-org/mlr3/issues/428
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, term("evals", n_evals = 3), TunerRandomSearch$new())
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
  at$train(task)
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
})
