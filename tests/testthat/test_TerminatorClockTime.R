context("TerminatorClockTime")

test_that("Time", {
  te = term("clock_time", secs = 1)
  inst = TEST_MAKE_INST1()
  inst$start_time = Sys.time() # as in "Tuner$tune()"
  expect_false(te$is_terminated(inst))

  Sys.sleep(0.1)
  expect_false(te$is_terminated(inst))

  Sys.sleep(1.2)
  expect_true(te$is_terminated(inst))
})
