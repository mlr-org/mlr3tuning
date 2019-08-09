context("TerminatorTime")

test_that("Time", {
  te = TerminatorTime$new(Sys.time() + 1)
  expect_terminator(te)
  expect_is(te$settings$time, "POSIXct")
  expect_false(te$is_terminated)

  te$eval_before(pe)
  Sys.sleep(0.1)
  te$eval_after(pe)
  expect_false(te$is_terminated)

  te$eval_before(pe)
  Sys.sleep(1)
  te$eval_after(pe)

  expect_true(te$is_terminated)
})
