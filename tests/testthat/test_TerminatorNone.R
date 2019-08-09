context("TerminatorNone")

test_that("API", {
  te = TerminatorNone$new()
  expect_terminator(te)
  expect_false(te$is_terminated)
})

