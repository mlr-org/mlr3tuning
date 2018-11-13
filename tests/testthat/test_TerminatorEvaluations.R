context("TerminatorEvaluations")

test_that("API", {
  te = TerminatorEvaluations$new("iter-terminator", 2)
  expect_identical(te$settings$max_evaluations, 2L)
  expect_identical(te$state$evals, 0L)

  ff = list(experiments = data.table())

  te$update_start(ff)
  expect_identical(te$state$evals, 0L)
  te$update_end(ff)
  expect_identical(te$state$evals, 0L)
  expect_false(te$terminated)
  expect_string(te$message, fixed = "0/2")

  ff = list(experiments = data.table(a = 1))

  te$update_start(ff)
  expect_identical(te$state$evals, 1L)
  te$update_end(ff)
  expect_identical(te$state$evals, 1L)
  expect_false(te$terminated)
  expect_string(te$message, fixed = "1/2")

  ff = list(experiments = data.table(a = 2))
  expect_string(te$message, "2/2")
})
