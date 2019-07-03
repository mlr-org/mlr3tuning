context("TerminatorRuntime")

test_that("API", {
  te = TerminatorRuntime$new(1)

  expect_identical(te$settings$runtime, 1)
  expect_identical(te$state$start, NULL)
  expect_false(te$terminated)

  pe = list(bmr = list(data = data.table()))

  te$update_start(pe)
  Sys.sleep(0.1)
  te$update_end(pe)

  expect_number(te$state$start)
  expect_true(te$state$remaining < 1)
  expect_false(te$terminated)

  te$update_start(pe)
  Sys.sleep(1)
  te$update_end(pe)

  expect_true(te$state$remaining < 0)
  expect_true(te$terminated)
})
