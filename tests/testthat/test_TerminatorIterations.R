context("TerminatorIterations")


test_that("API", {
  ti = TerminatorIterations$new(2)
  expect_identical(ti$settings$max_iterations, 2L)
  expect_identical(ti$state$iters, 0L)
  expect_string(format(ti), fixed = "2 remaining")

  ff = list(experiments = data.table())
  ti$update_start(ff)
  expect_identical(ti$state$iters, 0L)
  ti$update_end(ff)
  expect_identical(ti$state$iters, 0L)
  expect_false(ti$terminated)
  expect_string(format(ti), fixed = "2 remaining")

  ff = list(experiments = data.table(dob = 1L))
  ti$update_start(ff)
  ti$update_end(ff)
  expect_identical(ti$state$iters, 1L)

  ff = list(experiments = data.table(dob = 2L))
  ti$update_start(ff)
  ti$update_end(ff)
  expect_true(ti$terminated)

  expect_string(format(ti), fixed = "0 remaining")
})
