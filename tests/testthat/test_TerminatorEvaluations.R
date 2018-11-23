context("TerminatorEvaluations")

test_that("API", {
  te = TerminatorEvaluations$new(2)
  expect_identical(te$settings$max_evaluations, 2L)
  expect_identical(te$state$evals, 0L)

  ff = list(bmr = list(data = data.table()))

  te$update_start(ff)
  expect_identical(te$state$evals, 0L)
  te$update_end(ff)
  expect_identical(te$state$evals, 0L)
  expect_false(te$terminated)
  expect_string(format(te), fixed = "2 remaining")

  ff = list(bmr = list(data = data.table(hash = c(1,1))))

  te$update_start(ff)
  expect_identical(te$state$evals, 1L)
  te$update_end(ff)
  expect_identical(te$state$evals, 1L)
  expect_false(te$terminated)
  expect_string(format(te), fixed = "1 remaining")

  ff = list(experiments = data.table(hash = 1:2))
  expect_string(format(te), "0 remaining")
})
