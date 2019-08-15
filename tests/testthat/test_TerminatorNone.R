context("TerminatorNone")

test_that("API", {
  te = TerminatorNone$new()
  expect_false(te$is_terminated(NULL))
})
