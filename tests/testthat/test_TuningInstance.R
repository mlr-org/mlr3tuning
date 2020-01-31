context("TuningInstance")

test_that("TuningInstance", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L, measures = msr("dummy.cp.classif"), n_dim = 2)
  # test empty inst
  expect_data_table(inst$bmr$data, nrows = 0)
  expect_identical(inst$n_evals, 0L)
  expect_output(print(inst), "Not tuned")

  # add a couple of eval points and test the state of inst
  z = inst$eval_batch(data.table(cp = c(0.01, 0.02), minsplit = c(3, 4)))
  expect_data_table(inst$bmr$data, nrows = 4L)
  expect_equal(inst$bmr$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(inst$bmr$resample_result(1)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$bmr$resample_result(1)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$bmr$resample_result(2)$learners[[1]]$param_set$values$cp, 0.02)
  expect_equal(inst$bmr$resample_result(2)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$bmr$resample_result(2)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$n_evals, 2L)
  expect_output(print(inst), "Not tuned")
  expect_list(z, len = 3)
  expect_named(z, c("batch_nr", "uhashes", "perf"))
  expect_equal(z$batch_nr, 1L)
  expect_character(z$uhashes, len = 2L)
  expect_data_table(z$perf, nrows = 2L, ncols = 1L)
  expect_named(z$perf, "dummy.cp.classif")

  inst$eval_batch(data.table(cp = c(0.001, 0.001), minsplit = c(3, 4)))
  expect_data_table(inst$bmr$data, nrows = 8L)
  expect_equal(inst$bmr$resample_result(3)$learners[[1]]$param_set$values$cp, 0.001)
  expect_equal(inst$bmr$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(inst$bmr$resample_result(3)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(inst$bmr$resample_result(4)$learners[[1]]$param_set$values$cp, 0.001)
  expect_equal(inst$bmr$resample_result(4)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(inst$bmr$resample_result(4)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(inst$n_evals, 4L)
  expect_list(z, len = 3)
  expect_named(z, c("batch_nr", "uhashes", "perf"))
  expect_equal(z$batch_nr, 1L)
  expect_character(z$uhashes, len = 2L)
  expect_data_table(z$perf, nrows = 2L, ncols = 1L)
  expect_named(z$perf, "dummy.cp.classif")

  # test archive
  a = inst$archive(unnest = "no")
  expect_data_table(a, nrows = 4L)
  a = inst$archive(unnest = "params")
  expect_data_table(a, nrows = 4L)
  expect_true("cp" %in% colnames(a))
  expect_true("dummy.cp.classif" %in% colnames(a))
})


test_that("archive one row (#40)", {
  inst = TEST_MAKE_INST1()
  inst$eval_batch(data.table(cp = c(0.01)))
  a = inst$archive()
  expect_data_table(a, nrows = 1)
  expect_number(a$classif.ce)
})

test_that("eval_batch and termination", {
  inst = TEST_MAKE_INST1(term_evals = 3L)
  design = generate_design_random(inst$param_set, 2)$data
  inst$eval_batch(design[1:2, ])
  expect_data_table(inst$archive(), nrows = 2L)
  inst$eval_batch(design[1, ])
  expect_data_table(inst$archive(), nrows = 3L)
  expect_error(inst$eval_batch(design[1, ]), class = "terminated_error")
  expect_data_table(inst$archive(), nrows = 3L)

  inst = TEST_MAKE_INST1(term_evals = 5L)
  tuner = tnr("random_search", batch_size = 3L)
  tuner$tune(inst)
  expect_data_table(inst$archive(), nrows = 6L)

  # second start should be a NOP
  tuner$tune(inst)
  tab = inst$archive()
  expect_data_table(tab, nrows = 6L)
})

test_that("the same experiment can be added twice", {
  inst = TEST_MAKE_INST1()
  d = data.table(cp = c(0.1, 0.1))
  inst$eval_batch(d)
  tab = inst$archive()
  expect_data_table(tab, nrows = 2)
})


test_that("tuning with custom resampling", {
  task = tsk("pima")
  resampling = rsmp("custom")
  train_sets = list(1:300 , 332:632)
  test_sets = list(301:331, 633:663)
  resampling$instantiate(task, train_sets, test_sets)

  learner = lrn("classif.rpart")
  #resampling = rsmp("holdout")
  measures = msr("classif.ce")
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  terminator = term("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstance$new(task, learner, resampling, measures, tune_ps, terminator)
  tuner$tune(inst)
  rr = inst$bmr$resamplings
  expect_data_table(rr, nrows = 1)
  rr = rr$resampling[[1]]
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

  inst = TuningInstance$new(tsk("iris"),
    mlr3pipelines::po("select") %>>% lrn("classif.rpart"),
    rsmp("holdout"), msr("classif.ce"),
    paradox::ParamSet$new(list(
        paradox::ParamInt$new("classif.rpart.minsplit", 1, 1))),
    term("evals", n_evals=1))

  tnr("random_search")$tune(inst)
  expect_data_table(inst$archive("params"), nrows = 1)
})
