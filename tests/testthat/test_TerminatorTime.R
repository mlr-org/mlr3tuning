context("TerminatorTime")

test_that("Time", {
  te = TerminatorTime$new(Sys.time() + 1)
  expect_terminator(te)
  expect_is(te$settings$time, "POSIXct")
  expect_false(te$terminated)

  te$update_start(pe)
  Sys.sleep(0.1)
  te$update_end(pe)
  expect_false(te$terminated)

  te$update_start(pe)
  Sys.sleep(1)
  te$update_end(pe)

  expect_true(te$terminated)
})
