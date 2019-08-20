context("TerminatorClockTime")

test_that("Time", {
  te = TerminatorClockTime$new(secs = 1)
  pe = TEST_MAKE_INST1()
  pe$start_time = Sys.time() # as in "Tuner$tune()"
  expect_false(te$is_terminated(pe))

  Sys.sleep(0.1)
  expect_false(te$is_terminated(pe))

  Sys.sleep(1.2)
  expect_true(te$is_terminated(pe))
})
