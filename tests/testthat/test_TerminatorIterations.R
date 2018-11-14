context("TerminatorIterations")


test_that("API", {
  ti = TerminatorIterations$new(2)
  expect_identical(ti$settings$max_iterations, 2L)
  expect_identical(ti$state$iters, 0L)
  expect_string(format(ti), fixed = "2 remaining")

  ti$update_start()
  expect_identical(ti$state$iters, 0L)
  ti$update_end()
  expect_identical(ti$state$iters, 1L)
  expect_false(ti$terminated)
  expect_string(format(ti), fixed = "1 remaining")

  ti$update_start()
  expect_identical(ti$state$iters, 1L)
  ti$update_end()
  expect_identical(ti$state$iters, 2L)
  expect_true(ti$terminated)

  expect_string(format(ti), fixed = "0 remaining")
})
