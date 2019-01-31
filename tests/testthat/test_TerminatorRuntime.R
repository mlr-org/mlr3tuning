context("TerminatorRuntime")

test_that("API", {
  expect_error(TerminatorRuntime$new(1, "seconds"))
  te = TerminatorRuntime$new(1, "secs")

  expect_identical(te$settings$max_time, 1L)
  expect_identical(te$settings$units, "secs")

  expect_identical(te$state$time_start, NULL)
  expect_identical(te$state$time_end, NULL)
  expect_identical(te$state$time_remaining, 1L)
  
  expect_false(te$terminated)

  ff = list(bmr = list(data = data.table()))

  te$update_start(ff)
  Sys.sleep(0.1)
  te$update_end(ff)

  expect_true(te$state$time_start < te$state$time_end)
  expect_true(te$state$time_remaining < 1)
  expect_false(te$terminated)

  te$update_start(ff)
  Sys.sleep(1)
  te$update_end(ff)

  expect_true(te$state$time_start < te$state$time_end)
  expect_true(te$state$time_remaining < 0)
  expect_true(te$terminated)
})
