context("TerminatorTime")

test_that("Time", {
  te = TerminatorTime$new(Sys.time() + 1)
  pe = TEST_MAKE_PE1()
  expect_false(te$is_terminated(pe))

  Sys.sleep(0.1)
  expect_false(te$is_terminated(pe))

  Sys.sleep(1)
  expect_true(te$is_terminated(pe))
})
