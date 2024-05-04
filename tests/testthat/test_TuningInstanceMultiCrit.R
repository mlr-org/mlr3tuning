test_that("tuning with multiple objectives", {
  task = tsk("pima")
  resampling = rsmp("holdout")
  learner = lrn("classif.rpart")

  measure_ids = c("classif.fpr", "classif.tpr")
  measures = msrs(measure_ids)

  tune_ps = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10)
  )

  terminator = trm("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceBatchMultiCrit$new(task, learner, resampling, measures, terminator, tune_ps)

  tuner$optimize(inst)

  sp = inst$result_x_search_space
  obj = inst$result_y

  expect_names(names(sp), identical.to = tune_ps$ids())
  expect_data_table(sp, min.rows = 1, ncols = length(measures))
  expect_names(names(obj), identical.to = measure_ids)
  expect_data_table(inst$archive$data, nrows = 10L)
  expect_equal(inst$archive$cols_y, measure_ids)
  expect_data_table(inst$archive$best())
  expect_list(inst$result_x_domain)
})

test_that("store_benchmark_result and store_models flag works", {
  inst = TEST_MAKE_INST1_2D(store_benchmark_result = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_true("uhashes" %nin% colnames(inst$archive$data))

  inst = TEST_MAKE_INST1_2D(store_benchmark_result = TRUE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")

  inst = TEST_MAKE_INST1_2D(store_benchmark_result = TRUE, store_models = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_null(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  inst = TEST_MAKE_INST1_2D(store_benchmark_result = TRUE, store_models = TRUE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_class(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")
})

test_that("check_values flag with parameter set dependencies", {
  learner = LearnerRegrDepParams$new()
  learner$param_set$values$xx = "a"
  search_space = ps(
    cp = p_dbl(lower = 0.1, upper = 0.3),
    yy = p_dbl(lower = 0.1, upper = 0.3)
  )
  terminator = trm("evals", n_evals = 20)
  tuner = tnr("random_search")

  inst = TuningInstanceBatchMultiCrit$new(
    tsk("boston_housing"), learner,
    rsmp("holdout"), msrs(c("regr.mse", "regr.rmse")), terminator, search_space, check_values = FALSE)
  tuner$optimize(inst)
  expect_named(inst$result_learner_param_vals[[1]], c("xx", "cp", "yy"))

  inst = TuningInstanceBatchMultiCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msr("regr.mse"), terminator, search_space,
    check_values = TRUE)
  expect_error(tuner$optimize(inst),
    regexp = "yy.* can only be set")
})

test_that("search space from TuneToken works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$cp = to_tune(0.1, 0.3)

  instance = TuningInstanceBatchMultiCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measures = msrs(c("classif.acc", "classif.ce")),
    terminator = trm("evals", n_evals = 1))

  expect_r6(instance$search_space, "ParamSet")
  expect_equal(instance$search_space$ids(), "cp")

  ps = ps(
    cp = p_dbl(lower = 0.1, upper = 0.3)
  )

  expect_error(TuningInstanceBatchMultiCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measures = msrs(c("classif.acc", "classif.ce")),
    search_space = ps, terminator = trm("evals", n_evals = 1)),
    regexp = "If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.",
    fixed = TRUE)

  instance = TuningInstanceBatchMultiCrit$new(task = tsk("iris"),
    learner = lrn("classif.rpart"), resampling = rsmp("holdout"),
    measures = msrs(c("classif.acc", "classif.ce")), search_space = ps,
    terminator = trm("evals", n_evals = 1))

  expect_r6(instance$search_space, "ParamSet")
  expect_equal(instance$search_space$ids(), "cp")
})

test_that("TuneToken and result_learner_param_vals works", {
  learner = lrn("classif.rpart", xval = 0)
  learner$param_set$values$cp = to_tune(0.1, 0.3)

  instance = TuningInstanceBatchMultiCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 1))

  xdt = data.table(cp = 0.1)
  tuner = tnr("design_points", design = xdt)
  tuner$optimize(instance)

  expect_equal(instance$result_learner_param_vals[[1]]$xval, 0)
  expect_equal(instance$result_learner_param_vals[[1]]$cp, 0.1)
})

test_that("TuningInstanceBatchMultiCrit and empty search space works", {
  # xval constant
  instance = tune(
    tuner = tnr("random_search", batch_size = 5),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 10
  )

  expect_data_table(instance$result)
  expect_equal(instance$result$learner_param_vals[[1]], list(xval = 0))
  expect_equal(instance$result$x_domain[[1]], list())

  # xval and cp constant
  instance = tune(
    tuner = tnr("random_search", batch_size = 5),
    task = tsk("pima"),
    learner = lrn("classif.rpart", xval = 0, cp = 0.1),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 10
  )

  expect_data_table(instance$result)
  expect_equal(sortnames(instance$result$learner_param_vals[[1]]), list(xval = 0, cp = 0.1))
  expect_equal(instance$result$x_domain[[1]], list())

  # no constant
  learner = lrn("classif.rpart")
  learner$param_set$values$xval = NULL

  instance = tune(
    tuner = tnr("random_search", batch_size = 5),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    term_evals = 10
  )

  expect_data_table(instance$result)
  expect_equal(instance$result$learner_param_vals[[1]], list())
  expect_equal(instance$result$x_domain[[1]], list())
})

test_that("assign_result works", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))
  task = tsk("pima")
  resampling = rsmp("holdout")
  measures = msrs(c("classif.fpr", "classif.tpr"))
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchMultiCrit$new(task, learner, resampling, measures, terminator)

  xdt = data.table(cp = c(0.1, 0.01))
  ydt = data.table(classif.fpr = c(0.8, 0.7), classif.tpr = c(0.3, 0.2))

  instance$assign_result(xdt, ydt)
  res = instance$result
  expect_data_table(res, nrows = 2)
  expect_equal(res$cp, c(0.1, 0.01))
  expect_equal(res$classif.fpr, c(0.8, 0.7))
  expect_equal(res$classif.tpr, c(0.3, 0.2))
  expect_equal(sortnames(res$learner_param_vals[[1]]), list(xval = 0, cp = 0.1))
})
