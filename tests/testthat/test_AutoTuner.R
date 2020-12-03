test_that("AutoTuner / train+predict", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, ps)
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
  expect_s3_class(at$learner$model, "rpart")
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
  at = AutoTuner$new(lrn("classif.rpart", predict_type = "prob"), r_inner, ms, te, tuner, param_set)

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

test_that("Custom resampling is not allowed", {
  measure = msr("classif.ce")
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1()
  tuner = TunerRandomSearch$new()
  r = rsmp("holdout")$instantiate(task)
  expect_error(AutoTuner$new(lrn("classif.rpart"), r, measure, te, tuner, ps), "instantiated")
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
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 3), TunerRandomSearch$new(), ps)
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
  expect_s3_class(at$learner$model$classif.rpart$model, "rpart")
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

  expect_s3_class(tab$learner[[1]]$model$learner$model$classif.rpart$model, "rpart")
  expect_s3_class(tab$learner[[1]]$model$learner$model$classif.rpart$model, "rpart")
})

test_that("store_tuning_instance, store_benchmark_result and store_models flags work", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = msr("classif.ce")
  tuner = tnr("grid_search", resolution = 3)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = TRUE,
    store_models = TRUE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_class(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_null(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceSingleCrit")
  assert_null(at$tuning_instance$archive$benchmark_result)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_null(at$tuning_instance)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = TRUE,
    store_models = FALSE),
    regexp = "Benchmark results can only be stored if store_tuning_instance is set to TRUE",
    fixed = TRUE)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = FALSE,
    store_models = TRUE),
    regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
    fixed = TRUE)

  expect_error(AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = TRUE),
    regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
    fixed = TRUE)
})

test_that("predict_type works", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = msr("classif.ce")
  tuner = tnr("grid_search", resolution = 3)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps)

  at$train(task)
  expect_equal(at$predict_type, "response")
  expect_equal(at$model$learner$predict_type, "response")

  at$predict_type = "prob"
  expect_equal(at$predict_type, "prob")
  expect_equal(at$model$learner$predict_type, "prob")
})

test_that("search space from TuneToken works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$cp = to_tune(0.1, 0.3)

  at = AutoTuner$new(learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), terminator = trm("evals", n_evals = 1),
    tuner = tnr("random_search"))

  at$train(tsk("iris"))
  expect_equal(at$tuning_instance$search_space$ids(), "cp")

  ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.1, upper = 0.3)
  ))

  expect_error(AutoTuner$new(learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), terminator = trm("evals", n_evals = 1),
    tuner = tnr("random_search"), search_space = learner$param_set$search_space()),
    regexp = "If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.",
    fixed = TRUE)
})
