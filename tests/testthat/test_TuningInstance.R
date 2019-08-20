context("TuningInstance")

test_that("TuningInstance", {
  pe = TEST_MAKE_INST1(values = list(maxdepth = 10), folds = 2L, measures = "dummy.cp", n_dim = 2)
  # test empty PE
  expect_data_table(pe$bmr$data, nrows = 0)
  expect_identical(pe$n_evals, 0L)
  expect_output(print(pe), "Empty data.table")

  # add a couple of eval points and test the state of PE
  pe$eval_batch(data.table(cp = c(0.01, 0.02), minsplit = c(3, 4)))
  expect_data_table(pe$bmr$data, nrows = 4L)
  expect_equal(pe$bmr$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(pe$bmr$resample_result(1)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(pe$bmr$resample_result(1)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(pe$bmr$resample_result(2)$learners[[1]]$param_set$values$cp, 0.02)
  expect_equal(pe$bmr$resample_result(2)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(pe$bmr$resample_result(2)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(pe$n_evals, 2L)
  expect_output(print(pe), "0.02")

  pe$eval_batch(data.table(cp = c(0.001, 0.001), minsplit = c(3, 4)))
  expect_data_table(pe$bmr$data, nrows = 8L)
  expect_equal(pe$bmr$resample_result(3)$learners[[1]]$param_set$values$cp, 0.001)
  expect_equal(pe$bmr$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(pe$bmr$resample_result(3)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_equal(pe$bmr$resample_result(4)$learners[[1]]$param_set$values$cp, 0.001)
  expect_equal(pe$bmr$resample_result(4)$learners[[1]]$param_set$values$minsplit, 4)
  expect_equal(pe$bmr$resample_result(4)$learners[[1]]$param_set$values$maxdepth, 10)
  expect_identical(pe$n_evals, 4L)

  # test best-method
  b = pe$best(ties_method = "first")
  blrn = b$learners[[1L]]
  expect_resample_result(b)
  expect_identical(blrn$param_set$values, list(maxdepth = 10, cp = 0.001, minsplit = 3))
  b = pe$best(ties_method = "last")
  blrn = b$learners[[1L]]
  expect_resample_result(b)
  expect_identical(blrn$param_set$values, list(maxdepth = 10, cp = 0.001, minsplit = 4))

  # test archive
  a = pe$archive(unnest = FALSE)
  expect_data_table(a, nrows = 4L)
  a = pe$archive(unnest = TRUE)
  expect_data_table(a, nrows = 4L)
  expect_true("cp" %in% colnames(a))
  expect_true("dummy.cp" %in% colnames(a))
})


test_that("archive one row (#40)", {
  pe = TEST_MAKE_INST1()
  pe$eval_batch(data.table(cp = c(0.01)))
  a = pe$archive()
  expect_data_table(a, nrows = 1)
  expect_number(a$classif.ce)
})
