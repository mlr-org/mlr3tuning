test_that("AutoTuner / train+predict", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  ps = TEST_MAKE_PS1(n_dim = 1)
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$cp == 0.2) 0 else 1) # lets fake a measure, so we control the best config
  tuner = tnr("grid_search", resolution = 3)
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, ps)
  expect_false("marshal" %in% at$properties)
  expect_learner(at)
  at$train(task)
  expect_learner(at)

  expect_equal(sortnames(at$learner$param_set$values), list(xval = 0, cp = 0.2))
  inst = at$tuning_instance
  a = at$archive$data
  expect_data_table(a, nrows = 3L)
  r = at$tuning_result
  expect_equal(r$x_domain[[1]], list(cp = 0.2))
  expect_equal(sortnames(r$learner_param_vals[[1]]), list(xval = 0, cp = 0.2))
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
  at = AutoTuner$new(tuner, lrn("classif.rpart", predict_type = "prob"), r_inner, ms, te, param_set)

  expect_null(at$tuning_instance)
  expect_equal(at$predict_type, "prob")

  rr = resample(tsk("iris"), at, r_outer, store_models = TRUE)


  # check tuning results of all outer folds
  expect_equal(length(rr$learners), outer_folds)
  lapply(rr$learners, function(ll) {
    assert_r6(ll, "AutoTuner")
    expect_equal(sortnames(ll$learner$param_set$values), list(xval = 0, cp = 0.2))
    inst = ll$tuning_instance
    assert_r6(inst, "TuningInstanceBatchSingleCrit")
    expect_data_table(inst$archive$data, nrows = inner_evals)
    expect_numeric(inst$result_y, len = 1L)
  })
})

test_that("nested resamppling results are consistent ", {
  # we had a bad pointer bug due to missing cloning here
  # https://github.com/mlr-org/mlr3/issues/428
  # this resulted in different tuning results stored in models than used in final training

  ps = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10))


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
  at = AutoTuner$new(TunerBatchRandomSearch$new(), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 3), ps)
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
  at$train(task)
  expect_equal(at$instance_args$learner$param_set$values, list(xval = 0))
})

test_that("AutoTuner works with graphlearner", {
  skip_if_not_installed("mlr3pipelines")
  skip_if(packageVersion("mlr3pipelines") < "0.5.3")
  requireNamespace("mlr3pipelines")

  gl = MAKE_GL()
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$classif.rpart.cp == 0.2) 0 else 1)
  te = trm("evals", n_evals = 4)
  ps = ps(
    classif.rpart.cp = p_dbl(lower = 0.1, upper = 0.3)
  )
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
  a = at$archive$data
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
  skip_if(packageVersion("mlr3pipelines") < "0.5.3")
  requireNamespace("mlr3pipelines")

  gl = MAKE_GL()
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$classif.rpart.cp == 0.2) 0 else 1)
  te = trm("evals", n_evals = 4)
  ps = ps(
    classif.rpart.cp = p_dbl(lower = 0.1, upper = 0.3)
  )
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

  expect_data_table(tab$learner[[1]]$archive$data, nrows = 3L)
  expect_data_table(tab$learner[[2]]$archive$data, nrows = 3L)

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

  assert_r6(at$tuning_instance, "TuningInstanceBatchSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_class(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceBatchSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_null(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = TRUE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceBatchSingleCrit")
  expect_equal(at$tuning_instance$archive$benchmark_result$n_resample_results, 0)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_null(at$tuning_instance)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = FALSE,
    store_models = TRUE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceBatchSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_class(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    tuner = tuner, ps, store_tuning_instance = FALSE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$tuning_instance, "TuningInstanceBatchSingleCrit")
  assert_benchmark_result(at$tuning_instance$archive$benchmark_result)
  assert_null(at$tuning_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)
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

  ps = ps(
    cp = p_dbl(lower = 0.1, upper = 0.3)
  )

  expect_error(AutoTuner$new(learner = learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), terminator = trm("evals", n_evals = 1),
    tuner = tnr("random_search"), search_space = learner$param_set$search_space()),
    regexp = "If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.",
    fixed = TRUE)
})

test_that("AutoTuner get_base_learner method works", {
  skip_if_not_installed("mlr3pipelines")
  skip_if(packageVersion("mlr3pipelines") < "0.5.3")
  requireNamespace("mlr3pipelines")

  # simple learner
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  at = auto_tuner(
    tuner = tnr("random_search"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 1)
  at$train(tsk("pima"))

  expect_learner(at$base_learner())
  expect_equal(at$base_learner()$id, "classif.rpart")
  expect_learner(at$base_learner(recursive = 0))
  expect_equal(at$base_learner(recursive = 0)$id, "classif.rpart")

  # graph learner
  learner = as_learner(pipeline_robustify() %>>% lrn("classif.rpart"))
  learner$param_set$values$classif.rpart.cp = to_tune(1e-04, 1e-1, logscale = TRUE)
  learner$id = "graphlearner.classif.rpart"

  at = auto_tuner(
    tuner = tnr("random_search"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 1)
  at$train(tsk("pima"))

  expect_learner(at$base_learner(recursive = 0))
  expect_equal(at$base_learner(recursive = 0)$id, "graphlearner.classif.rpart")
  # expect_learner(at$base_learner())
  # expect_equal(at$base_learner()$id, "classif.rpart")
})

test_that("AutoTuner hash works #647 in mlr3", {
  # different measure -> different hash?
  at_1 = AutoTuner$new(
    learner = lrn("classif.rpart", minsplit = to_tune(1, 12)),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 3))

  at_2 = AutoTuner$new(
    learner = lrn("classif.rpart", minsplit = to_tune(1, 12)),
    resampling = rsmp("holdout"),
    measure = msr("classif.acc"),
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 3))

  expect_true(at_1$hash != at_2$hash)


  at_1 = AutoTuner$new(
    learner = lrn("classif.rpart", minsplit = to_tune(1, 12)),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 3))

  at_2 = AutoTuner$new(
    learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1)),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 3))

  expect_true(at_1$hash != at_2$hash)

  resampling_outer = rsmp("holdout")
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  expect_data_table(bmr$learners, nrows = 2)
  expect_named(bmr$resample_result(1)$learners[[1]]$tuning_result, c("minsplit", "learner_param_vals", "x_domain", "classif.ce"))
  expect_named(bmr$resample_result(2)$learners[[1]]$tuning_result, c("cp", "learner_param_vals", "x_domain", "classif.ce"))
})

test_that("AutoTuner works with empty search space", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 5),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 10
  )

  at$train(tsk("pima"))
  expect_equal(at$tuning_instance$result$learner_param_vals[[1]], list(xval = 0))
  expect_equal(at$tuning_instance$result$x_domain, list(list()))

  # no constant
  learner = lrn("classif.rpart")
  learner$param_set$values$xval = NULL

  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 5),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 10
  )

  at$train(tsk("pima"))
  expect_list(at$tuning_instance$result$learner_param_vals[[1]], len = 0)
  expect_equal(at$tuning_instance$result$x_domain, list(list()))
})

test_that("AutoTuner importance method works", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  expect_error(at$importance(), "No model stored")
  at$train(tsk("penguins"))
  expect_numeric(at$importance(), len = 5)
})

test_that("AutoTuner selected_features method works", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  expect_error(at$selected_features(), "No model stored")
  at$train(tsk("penguins"))
  expect_character(at$selected_features())
})

test_that("AutoTuner oob_error method works", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  expect_error(at$oob_error(), "cannot calculate the out-of-bag error.")
})

test_that("AutoTuner loglik method works", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  expect_error(at$loglik(), "cannot calculate the log-likelihood.")
})


test_that("AutoTuner works with instantiated resampling", {
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  task = tsk("penguins")

  resampling_inner = rsmp("custom")
  resampling_inner$instantiate(task,
    train_sets = list(c(1:10, 161:170, 281:290)),
    test_sets = list(c(11:20, 171:180, 291:300))
  )

  at = auto_tuner(
    tuner =  tnr("random_search"),
    learner = learner,
    resampling = resampling_inner,
    measure = msr("classif.ce"),
    term_evals = 4)

  at$train(task)
  expect_data_table(at$tuning_instance$result, nrows = 1)
})

test_that("AutoTuner errors when train set is not a subset of task ids", {
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  task = tsk("penguins")
  task$filter(seq(20))

  resampling_inner = rsmp("custom")
  resampling_inner$instantiate(task,
    train_sets = list(11:15),
    test_sets = list(1:5)
  )

  at = auto_tuner(
    tuner =  tnr("random_search"),
    learner = learner,
    resampling = resampling_inner,
    measure = msr("classif.ce"),
    term_evals = 4)

  resampling_outer = rsmp("custom")
  resampling_outer$instantiate(task,
    train_sets = list(1:10),
    test_sets = list(11:20)
  )

  expect_error(resample(task, at, resampling_outer, store_models = TRUE), "Train set 1")
})

test_that("AutoTuner errors when second train set is not a subset of task ids", {
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  task = tsk("penguins")
  task$filter(seq(40))

  resampling_inner = rsmp("custom")
  resampling_inner$instantiate(task,
    train_sets = list(1:10, 21:30),
    test_sets = list(31:40, 11:20)
  )

  at = auto_tuner(
    tuner =  tnr("random_search"),
    learner = learner,
    resampling = resampling_inner,
    measure = msr("classif.ce"),
    term_evals = 4)

  resampling_outer = rsmp("custom")
  resampling_outer$instantiate(task,
    train_sets = list(1:20),
    test_sets = list(21:40)
  )

  expect_error(resample(task, at, resampling_outer, store_models = TRUE), "Train set 2")
})

test_that("AutoTuner errors when test set is not a subset of task ids", {
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  task = tsk("penguins")
  task$filter(seq(20))

  resampling_inner = rsmp("custom")
  resampling_inner$instantiate(task,
    train_sets = list(1:5),
    test_sets = list(11:16)
  )

  at = auto_tuner(
    tuner =  tnr("random_search"),
    learner = learner,
    resampling = resampling_inner,
    measure = msr("classif.ce"),
    term_evals = 4)

  resampling_outer = rsmp("custom")
  resampling_outer$instantiate(task,
    train_sets = list(1:10),
    test_sets = list(11:20)
  )

  expect_error(resample(task, at, resampling_outer, store_models = TRUE), "Test set 1")
})

test_that("AutoTuner errors when second test set is not a subset of task ids", {
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
  task = tsk("penguins")
  task$filter(seq(40))

  resampling_inner = rsmp("custom")
  resampling_inner$instantiate(task,
    train_sets = list(1:10, 11:20),
    test_sets = list(11:20, 21:30)
  )

  at = auto_tuner(
    tuner =  tnr("random_search"),
    learner = learner,
    resampling = resampling_inner,
    measure = msr("classif.ce"),
    term_evals = 4)

  resampling_outer = rsmp("custom")
  resampling_outer$instantiate(task,
    train_sets = list(1:20),
    test_sets = list(21:40)
  )

  expect_error(resample(task, at, resampling_outer, store_models = TRUE), "Test set 2")
})

# Marshal ----------------------------------------------------------------------

test_that("marshalable learner", {
  task = tsk("iris")
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4,
    store_tuning_instance = TRUE
  )
  expect_true("marshal" %in% at$properties)

  at$train(task)
  at$marshal()
  at$unmarshal()
  expect_false(at$learner$marshaled)

  expect_learner(at, task = task)
})

test_that("marshal", {
  task = tsk("iris")
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4,
    store_tuning_instance = TRUE
  )

  at$train(task)

  model = at$model
  model1 = marshal_model(model)
  model2 = unmarshal_model(model1)
  expect_false(model$learner$marshaled)
  expect_true(model1$marshaled$learner$marshaled)
  expect_false(model2$learner$marshaled)
  expect_equal(class(model), class(model2))
  expect_class(model1, "marshaled")
})

# Async ------------------------------------------------------------------------

test_that("AutoTuner works with async tuner", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  at = auto_tuner(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  at$train(tsk("pima"))

  expect_data_table(at$tuning_instance$result, nrows = 1)
  expect_data_table(at$tuning_instance$archive$data, min.rows = 4)
  expect_rush_reset(at$tuning_instance$rush, type = "terminate")
})

# Internal ---------------------------------------------------------------------

test_that("internal tuning and validation", {
  # we can use the internally optimized values for the final model fit
  task = tsk("iris")
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug", iter = to_tune(1, 1000L, aggr = function(x) length(x)), x = to_tune(0.2, 0.3),
      early_stopping = TRUE, validate = "test"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )
  at$train(task)
  expect_equal(at$model$learner$param_set$values$iter, 3)
  expect_false(at$model$learner$param_set$values$early_stopping)

  # the AutoTuner's validate field controls the validation data for the final model fit,
  # because it was set to NULL, the full data was used for the final model fit
  expect_true(is.null(at$model$learner$state$internal_valid_task_ids))

  # we can also still do early stopping on the final model fit if we want to
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug", iter = 1000L, x = to_tune(0.2, 0.3), early_stopping = TRUE, validate = "test"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )
  expect_error(at$train(task), "when a validation task is present")
  at$validate = 0.2
  at$train(task)

  # early stopping was not disabled
  expect_equal(at$model$learner$param_set$values$iter, 1000)
  expect_true(at$model$learner$param_set$values$early_stopping)
})

test_that("set_validate", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug", iter = 1000L, x = to_tune(0.2, 0.3), early_stopping = TRUE),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )
  set_validate(at, validate = "test", final_validate = 0.3)
  expect_equal(at$validate, 0.3)
  expect_equal(at$learner$validate, "test")
  set_validate(at, validate = NULL)
  expect_equal(at$validate, 0.3)
})

test_that("validate field can only be set if the tuned learner supports validation", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug", iter = 1000L, x = to_tune(0.2, 0.3), early_stopping = TRUE),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4,
    validate = 0.3
  )
  expect_equal(at$validate, 0.3)

  expect_error(
    auto_tuner(
      tuner = tnr("random_search", batch_size = 2),
      learner = lrn("classif.rpart"),
      resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      term_evals = 4,
      validate = 0.3
    ),
    "The learner that is tuned by"
  )
})

test_that("validation set can be used for final model fit", {
  at = auto_tuner(
    tuner = tnr("random_search"),
    learner = lrn("classif.debug", early_stopping = TRUE, x = to_tune(0, 1)),
    resampling = rsmp("holdout"),
    store_models = TRUE,
    term_evals = 1
  )

  task = tsk("iris")
  task$divide(ratio = 0.2)

  set_validate(at, final_validate = "predefined", validate = "predefined")
  at$train(task)
  expect_equal(at$state$internal_valid_task_hash, task$internal_valid_task$hash)
  expect_equal(at$state$validate, "predefined")
})
