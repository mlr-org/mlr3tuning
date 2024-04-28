test_that("TuningInstanceBatchSingleCrit", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L, measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2)
  # test empty inst
  expect_data_table(inst$archive$data, nrows = 0)
  expect_identical(inst$archive$n_evals, 0L)
  #expect_output(print(inst), "Not tuned")

  # add a couple of eval points and test the state of inst
  z = inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_data_table(inst$archive$data, nrows = 2L)

  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp, 0.3)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$cp, 0.25)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2)
  expect_named(z, "dummy.cp.classif")

  z = inst$eval_batch(data.table(cp = c(0.2, 0.1), minsplit = c(3, 4)))
  expect_data_table(inst$archive$data, nrows = 4L)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$cp, 0.2)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$cp, 0.1)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, "dummy.cp.classif")

  # test archive
  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
  a = as.data.table(inst$archive)
  expect_data_table(a, nrows = 4L)
  expect_true("x_domain_cp" %in% colnames(a))
  expect_true("dummy.cp.classif" %in% colnames(a))
})


test_that("archive one row (#40)", {
  inst = TEST_MAKE_INST1()
  inst$eval_batch(data.table(cp = 0.1))
  a = inst$archive$data
  expect_data_table(a, nrows = 1)
  expect_number(a$classif.ce)
})

test_that("eval_batch and termination", {
  inst = TEST_MAKE_INST1(term_evals = 3L)
  design = generate_design_random(inst$search_space, 2)$data
  inst$eval_batch(design[1:2, ])
  expect_data_table(inst$archive$data, nrows = 2L)
  inst$eval_batch(design[1, ])
  expect_data_table(inst$archive$data, nrows = 3L)
  expect_error(inst$eval_batch(design[1, ]), class = "terminated_error")
  expect_data_table(inst$archive$data, nrows = 3L)

  inst = TEST_MAKE_INST1(term_evals = 5L)
  tuner = tnr("random_search", batch_size = 3L)
  tuner$optimize(inst)
  expect_data_table(inst$archive$data, nrows = 6L)

  # second start should be a NOP
  tuner$optimize(inst)
  tab = inst$archive$data
  expect_data_table(tab, nrows = 6L)
})

test_that("the same experiment can be added twice", {
  inst = TEST_MAKE_INST1()
  d = data.table(cp = c(0.1, 0.1))
  inst$eval_batch(d)
  tab = inst$archive$data
  expect_data_table(tab, nrows = 2)
})


test_that("tuning with custom resampling", {
  task = tsk("pima")
  resampling = rsmp("custom")
  train_sets = list(1:300 , 332:632)
  test_sets = list(301:331, 633:663)
  resampling$instantiate(task, train_sets, test_sets)

  learner = lrn("classif.rpart")
  measure = msr("classif.ce")
  tune_ps = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10)
  )
  terminator = trm("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator, tune_ps)
  tuner$optimize(inst)
  rr = as.data.table(inst$archive$benchmark_result)$resampling
  expect_list(rr, len = 20)
  rr = inst$archive$benchmark_result$resample_result(1)$resampling
  expect_equal(rr$iters, 2)
  expect_set_equal(rr$train_set(1), train_sets[[1]])
  expect_set_equal(rr$train_set(2), train_sets[[2]])
  expect_set_equal(rr$test_set(1), test_sets[[1]])
  expect_set_equal(rr$test_set(2), test_sets[[2]])
})

test_that("non-scalar hyperpars (#201)", {
  skip_if_not_installed("mlr3pipelines")

  requireNamespace("mlr3pipelines")
  `%>>%` = getFromNamespace("%>>%", asNamespace("mlr3pipelines"))


  learner = mlr3pipelines::po("select") %>>% lrn("classif.rpart")

  search_space = ps(
    classif.rpart.minsplit = p_int(1, 1),
    .extra_trafo = function(x, param_set) {
      x$select.selector = mlr3pipelines::selector_all()
      return(x)
    }
  )

  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), learner, rsmp("holdout"),
    msr("classif.ce"), trm("evals", n_evals = 1), search_space,
    check_values = FALSE)

  tnr("random_search")$optimize(inst)
  expect_data_table(inst$archive$data, nrows = 1)
})

test_that("store_benchmark_result and store_models flag works", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_true("uhashes" %nin% colnames(inst$archive$data))

  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = TRUE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")

  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = TRUE, store_models = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_null(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = TRUE, store_models = TRUE)
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

  inst = TuningInstanceBatchSingleCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msr("regr.mse"), terminator, search_space, check_values = FALSE)
  tuner$optimize(inst)
  expect_named(inst$result_learner_param_vals, c("xx", "cp", "yy"))

  inst = TuningInstanceBatchSingleCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msr("regr.mse"), terminator, search_space,
    check_values = TRUE)
  expect_error(tuner$optimize(inst),
    regexp = "yy.* can only be set")
})

test_that("search space from TuneToken works", {
  learner = lrn("classif.rpart")
  learner$param_set$values$cp = to_tune(0.1, 0.3)

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1))

  expect_r6(instance$search_space, "ParamSet")
  expect_equal(instance$search_space$ids(), "cp")

  ps = ps(
    cp = p_dbl(lower = 0.1, upper = 0.3)
  )

  expect_error(TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measure = msr("classif.ce"),
    search_space = ps, terminator = trm("evals", n_evals = 1)),
    regexp = "If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.",
    fixed = TRUE)

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"),
    learner = lrn("classif.rpart"), resampling = rsmp("holdout"),
    measure = msr("classif.ce"), search_space = ps,
    terminator = trm("evals", n_evals = 1))

  expect_r6(instance$search_space, "ParamSet")
  expect_equal(instance$search_space$ids(), "cp")
})

test_that("TuneToken and result_learner_param_vals works", {
  learner = lrn("classif.rpart", xval = 0)
  learner$param_set$values$cp = to_tune(0.1, 0.3)

  instance = TuningInstanceBatchSingleCrit$new(task = tsk("iris"), learner = learner,
    resampling = rsmp("holdout"), measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1))

  xdt = data.table(cp = 0.1)
  tuner = tnr("design_points", design = xdt)
  tuner$optimize(instance)

  expect_equal(instance$result_learner_param_vals$xval, 0)
  expect_equal(instance$result_learner_param_vals$cp, 0.1)
})


test_that("TuningInstanceBatchSingleCrit and empty search space works", {

  # xval constant
  instance = tune(
    tuner = tnr("random_search", batch_size = 5),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 10
  )

  expect_data_table(instance$result)
  expect_equal(instance$result$learner_param_vals, list(list(xval = 0)))
  expect_equal(instance$result$x_domain, list(list()))

  # no constant
  learner = lrn("classif.rpart")
  learner$param_set$values$xval = NULL

  instance = tune(
    tuner = tnr("random_search", batch_size = 5),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 10
  )

  expect_data_table(instance$result)
  expect_list(instance$result$learner_param_vals[[1]], len = 0)
  expect_equal(instance$result$x_domain, list(list()))
})

test_that("assign_result works with one hyperparameter", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))
  learner$param_set$values$xval = NULL
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table(cp = 0.1)
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$cp, 0.1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(res$learner_param_vals[[1]], list(cp = 0.1))
})

test_that("assign_result works with two hyperparameters", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minbucket = to_tune(1, 12))
  learner$param_set$values$xval = NULL
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table(cp = 0.1, minbucket = 1)
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$cp, 0.1)
  expect_equal(res$minbucket, 1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(res$learner_param_vals[[1]], list(cp = 0.1, minbucket = 1))
})

test_that("assign_result works with two hyperparameters and one constant", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), minbucket = to_tune(1, 12), xval = 1)
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table(cp = 0.1, minbucket = 1)
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$cp, 0.1)
  expect_equal(res$minbucket, 1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(res$learner_param_vals[[1]], list(xval = 1, cp = 0.1, minbucket = 1))
})

test_that("assign_result works with no hyperparameters and one constant", {
  learner = lrn("classif.rpart", xval = 1)
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table()
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(res$learner_param_vals[[1]], list(xval = 1))
})


test_that("assign_result works with no hyperparameters and two constant", {
  learner = lrn("classif.rpart", xval = 1, cp = 1)
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table()
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(sortnames(res$learner_param_vals[[1]]), list(xval = 1, cp = 1))
})

test_that("assign_result works with one hyperparameters and one constant", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1), xval = 1)
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table(cp = 0.1)
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$cp, 0.1)
  expect_equal(res$classif.ce, 0.8)
  expect_equal(res$learner_param_vals[[1]], list(xval = 1, cp = 0.1))
})

test_that("assign_result works with no hyperparameter and constant", {
  learner = lrn("classif.rpart")
  learner$param_set$values$xval = NULL
  task = tsk("pima")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = TuningInstanceBatchSingleCrit$new(task, learner, resampling, measure, terminator)

  xdt = data.table()
  y = c(classif.ce = 0.8)

  instance$assign_result(xdt, y)
  res = instance$result
  expect_data_table(res, nrows = 1)
  expect_equal(res$classif.ce, 0.8)
  expect_list(res$learner_param_vals[[1]], len = 0)
})
