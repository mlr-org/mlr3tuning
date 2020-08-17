context("TuningInstanceSingleCrit")

test_that("TuningInstanceSingleCrit", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L, measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2)
  # test empty inst
  expect_data_table(inst$archive$data(), nrows = 0)
  expect_identical(inst$archive$n_evals, 0L)
  #expect_output(print(inst), "Not tuned")

  # add a couple of eval points and test the state of inst
  z = inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_data_table(inst$archive$data(), nrows = 2L)

  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$cp, 0.3)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$cp, 0.25)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2)
  expect_named(z, c("dummy.cp.classif", "uhash"))

  z = inst$eval_batch(data.table(cp = c(0.2, 0.1), minsplit = c(3, 4)))
  expect_data_table(inst$archive$data(), nrows = 4L)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$cp, 0.2)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$cp, 0.1)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, c("dummy.cp.classif", "uhash"))

  # test archive
  a = inst$archive$data()
  expect_data_table(a, nrows = 4L)
  a = inst$archive$data(unnest = "x_domain")
  expect_data_table(a, nrows = 4L)
  expect_true("x_domain_cp" %in% colnames(a))
  expect_true("dummy.cp.classif" %in% colnames(a))
})


test_that("archive one row (#40)", {
  inst = TEST_MAKE_INST1()
  inst$eval_batch(data.table(cp = 0.1))
  a = inst$archive$data()
  expect_data_table(a, nrows = 1)
  expect_number(a$classif.ce)
})

test_that("eval_batch and termination", {
  inst = TEST_MAKE_INST1(term_evals = 3L)
  design = generate_design_random(inst$search_space, 2)$data
  inst$eval_batch(design[1:2, ])
  expect_data_table(inst$archive$data(), nrows = 2L)
  inst$eval_batch(design[1, ])
  expect_data_table(inst$archive$data(), nrows = 3L)
  expect_error(inst$eval_batch(design[1, ]), class = "terminated_error")
  expect_data_table(inst$archive$data(), nrows = 3L)

  inst = TEST_MAKE_INST1(term_evals = 5L)
  tuner = tnr("random_search", batch_size = 3L)
  tuner$optimize(inst)
  expect_data_table(inst$archive$data(), nrows = 6L)

  # second start should be a NOP
  tuner$optimize(inst)
  tab = inst$archive$data()
  expect_data_table(tab, nrows = 6L)
})

test_that("the same experiment can be added twice", {
  inst = TEST_MAKE_INST1()
  d = data.table(cp = c(0.1, 0.1))
  inst$eval_batch(d)
  tab = inst$archive$data()
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
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  terminator = trm("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceSingleCrit$new(task, learner, resampling, measure, tune_ps, terminator)
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

  search_space = ParamSet$new(list(
    ParamInt$new("classif.rpart.minsplit", 1, 1)))
  search_space$trafo = function(x, param_set) {
    x$select.selector = mlr3pipelines::selector_all()
    return(x)
  }

  inst = TuningInstanceSingleCrit$new(tsk("iris"),
    learner,
    rsmp("holdout"), msr("classif.ce"),
    search_space,
    trm("evals", n_evals = 1), check_values = TRUE)

  tnr("random_search")$optimize(inst)
  expect_data_table(inst$archive$data(), nrows = 1)
})

test_that("store_benchmark_result flag works", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_true("uhashes" %nin% colnames(inst$archive$data()))

  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L,
    measure = msr("dummy.cp.classif", fun = function(pv) pv$cp), n_dim = 2,
    store_benchmark_result = TRUE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")
})

test_that("check_values flag with parameter set dependencies", {
  learner = LearnerRegrDepParams$new()
  learner$param_set$values$xx = "a"
  search_space = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.1, upper = 0.3),
    ParamDbl$new("yy", lower = 0.1, upper = 0.3)
  ))
  terminator = trm("evals", n_evals = 20)
  tuner = tnr("random_search")

  inst = TuningInstanceSingleCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msr("regr.mse"), search_space, terminator)
  tuner$optimize(inst)
  expect_named(inst$result_learner_param_vals, c("xx", "cp", "yy"))

  inst = TuningInstanceSingleCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msr("regr.mse"), search_space, terminator,
    check_values = TRUE)
  expect_error(tuner$optimize(inst),
    fixed = "The parameter 'yy' can only be set")
})
