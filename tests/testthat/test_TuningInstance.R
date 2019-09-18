context("TuningInstance")

test_that("TuningInstance", {
  inst = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L, measures = msr("dummy.cp.classif"), n_dim = 2)
  # test empty inst
  expect_data_table(inst$bmr$data, nrows = 0)
  expect_identical(inst$n_evals, 0L)
  expect_output(print(inst), "Empty data.table")

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
  expect_output(print(inst), "0.02")
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

test_that("budget", {
  inst = TEST_MAKE_INST1()
  design = generate_design_random(inst$param_set, 8)$data
  expect_error(inst$eval_batch(design[1:6, ]), class = "terminated_error")
  tab = inst$archive()
  expect_data_table(tab, nrows = 6)

  inst = TEST_MAKE_INST1()
  tuner = tnr("random_search", batch_size = 6)
  tuner$tune(inst)
  tab = inst$archive()
  expect_data_table(tab, nrows = 6)

  # second start should be a NOP
  tuner$tune(inst)
  tab = inst$archive()
  expect_data_table(tab, nrows = 6)
})

test_that("the same experiment can be added twice", {
  inst = TEST_MAKE_INST1()
  d = data.table(cp = c(0.1, 0.1))
  inst$eval_batch(d)
  tab = inst$archive()
  expect_data_table(tab, nrows = 2)
})

