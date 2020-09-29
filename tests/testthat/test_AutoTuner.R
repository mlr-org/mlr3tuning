context("AutoTuner")

test_that("AutoTuner / train+predict", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
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

  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
  tuner = tnr("grid_search", resolution = 3)
  r_inner = rsmp("holdout")
  r_outer = rsmp("cv", folds = 2)
  param_set = TEST_MAKE_PS1()
  te = trm("evals", n_evals = inner_evals)
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(lrn("classif.rpart", predict_type = "prob"), r_inner, ms, param_set, te, tuner)

  expect_null(at$tuning_instance)
  expect_equal(at$predict_type, "prob")

  rr = resample(tsk("iris"), at, r_outer, store_models = TRUE)

  # check tuning results of all outer folds
  expect_equal(length(rr$learners), outer_folds)
  lapply(rr$learners, function(ll) {
    assert_r6(ll, "AutoTuner")
    expect_equal(ll$learner$param_set$values, list(xval = 0, cp = 0.2))
    inst = ll$tuning_instance
    assert_r6(inst, "TuningInstanceSingleCrit")
    expect_data_table(inst$archive$data(), nrows = inner_evals)
    expect_numeric(inst$result_y, len = 1L)
  })
})

# we had an issue that the AutoTuner did not return statically configured param in its result
# see issue #51
test_that("AutoTuner / param_set", {
  measure = msr("classif.ce")
  te = trm("evals", n_evals = 3)
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
  te = trm("evals", n_evals = 4)
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
    terminator = trm("evals", n_evals = 4),
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
  # https://github.com/mlr-org/mlr3/issues/428
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, trm("evals", n_evals = 3), TunerRandomSearch$new())
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
  at$train(task)
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
})

test_that("AutoTuner works with graphlearner", {
  skip_if_not_installed("mlr3pipelines")
  requireNamespace("mlr3pipelines")

  gl = MAKE_GL()
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$classif.rpart.cp == 0.2) 0 else 1)
  te = trm("evals", n_evals = 4)
  ps = ParamSet$new(list(
    ParamDbl$new("classif.rpart.cp", lower = 0.1, upper = 0.3)
  ))
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(
    learner = gl,
    resampling = rsmp("holdout"),
    measure = ms,
    search_space = ps,
    terminator = te,
    tuner = tuner)

  expect_learner(at)
  at$train(task)
  expect_learner(at)
  expect_equal(at$learner$param_set$values$classif.rpart.xval, 0)
  expect_equal(at$learner$param_set$values$classif.rpart.cp, 0.2)
  inst = at$tuning_instance
  a = at$archive$data()
  expect_data_table(a, nrows = 3L)
  r = at$tuning_result
  expect_equal(r$x_domain[[1]], list(classif.rpart.cp = 0.2))
  expect_equal(r$learner_param_vals[[1]]$classif.rpart.xval, 0)
  expect_equal(r$learner_param_vals[[1]]$classif.rpart.cp, 0.2)
  prd = at$predict(task)
  expect_prediction(prd)
  expect_is(at$learner$model$classif.rpart$model, "rpart")
})

test_that("Nested resampling works with graphlearner", {
  skip_if_not_installed("mlr3pipelines")
  requireNamespace("mlr3pipelines")

  gl = MAKE_GL()
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$classif.rpart.cp == 0.2) 0 else 1)
  te = trm("evals", n_evals = 4)
  ps = ParamSet$new(list(
    ParamDbl$new("classif.rpart.cp", lower = 0.1, upper = 0.3)
  ))
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(
    learner = gl,
    resampling = rsmp("holdout"),
    measure = ms,
    search_space = ps,
    terminator = te,
    tuner = tuner,
    store_tuning_instance = TRUE)

  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)
  tab = as.data.table(rr)

  expect_learner(tab$learner[[1]])
  expect_learner(tab$learner[[2]])

  expect_equal(tab$learner[[1]]$tuning_result$classif.rpart.cp, 0.2)
  expect_equal(tab$learner[[2]]$tuning_result$classif.rpart.cp, 0.2)

  expect_equal(tab$learner[[1]]$learner$param_set$values$classif.rpart.cp, 0.2)
  expect_equal(tab$learner[[2]]$learner$param_set$values$classif.rpart.cp, 0.2)

  expect_data_table(tab$learner[[1]]$archive$data(), nrows = 3L)
  expect_data_table(tab$learner[[2]]$archive$data(), nrows = 3L)

  expect_is(tab$learner[[1]]$model$learner$model$classif.rpart$model, "rpart")
  expect_is(tab$learner[[1]]$model$learner$model$classif.rpart$model, "rpart")
})

test_that("store_tuning_instance, store_benchmark_result and store_models flags work", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = msr("classif.ce")
  tuner = tnr("grid_search", resolution = 3)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = TRUE, store_benchmark_result = TRUE,
    store_models = TRUE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_class(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = TRUE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_null(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = TRUE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_null(at$tuning_instance$archive$benchmark_result)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_null(at$tuning_instance)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = FALSE, store_benchmark_result = TRUE,
    store_models = FALSE),
    regexp = "Benchmark results can only be stored if store_tuning_instance is set to TRUE",
    fixed = TRUE)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = TRUE, store_benchmark_result = FALSE,
    store_models = TRUE),
    regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
    fixed = TRUE)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, ps, te,
    tuner = tuner, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = TRUE),
    regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
    fixed = TRUE)
})
