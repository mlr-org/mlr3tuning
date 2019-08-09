context("TerminatorRuntime")

test_that("API", {
  te = TerminatorRuntime$new(1)
  expect_terminator(te)

  expect_identical(te$settings$runtime, 1)
  expect_identical(te$time_start, NULL)
  expect_false(te$is_terminated)

  pe = list(bmr = list(data = data.table()))

  te$eval_before(pe)
  Sys.sleep(0.1)
  te$eval_after(pe)

  expect_number(te$time_start)
  expect_false(te$is_terminated)

  te$eval_before(pe)
  Sys.sleep(1)
  te$eval_after(pe)

  expect_true(te$is_terminated)
})
