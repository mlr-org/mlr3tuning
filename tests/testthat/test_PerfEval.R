context("PerfEval")

test_that("PerfEval", {
  pe = TEST_MAKE_PE1(values = list(minsplit = 3), folds = 2L)

  # test empty PE
  expect_null(pe$bmr)
  expect_identical(pe$n_evals, 0L)

  # add a couple of eval points and test the state of PE
  pe$eval_batch(data.table(cp = c(0.01, 0.02)))
  expect_data_table(pe$bmr$data, nrows = 4L)
  expect_equal(pe$bmr$resample_result(1)$learners[[1]]$param_set$values$cp, 0.01)
  expect_equal(pe$bmr$resample_result(1)$learners[[1]]$param_set$values$minsplit, 3)
  expect_equal(pe$bmr$resample_result(2)$learners[[1]]$param_set$values$cp, 0.02)
  expect_identical(pe$n_evals, 2L)

  pe$eval_batch(data.table(cp = 0.1))
  expect_data_table(pe$bmr$data, nrows = 6L)
  expect_equal(pe$bmr$resample_result(3)$learners[[1]]$param_set$values$cp, 0.1)
  expect_equal(pe$bmr$resample_result(3)$learners[[1]]$param_set$values$minsplit, 3)
  expect_identical(pe$n_evals, 3L)

  expect_resample_result(pe$best())

  a = pe$archive(unnest = FALSE )
  expect_data_table(a, nrows = 3L)
  a = pe$archive(unnest = TRUE)
  expect_data_table(a, nrows = 3L)
  expect_true("cp" %in% colnames(a))
  expect_true("classif.ce" %in% colnames(a))
})


test_that("hooks", {
  pe = TEST_MAKE_PE1(values = list(minsplit = 3), folds = 2L)

  expect_error(pe$add_hook(hook = mean))
  expect_silent(pe$add_hook(hook = function(pe) {
    "hook"
  }))
  expect_equal(pe$run_hooks(), list("hook"))
})

test_that("archive one row (#40)", {
  pe = TEST_MAKE_PE1()
  pe$eval_batch(data.table(cp = c(0.01)))
  a = pe$archive()
  expect_data_table(a, nrows = 1)
  expect_number(a$classif.ce)
})

