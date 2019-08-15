context("TerminatorCombo")

test_that("API", {
  ti = TerminatorEvals$new(2)
  tr = TerminatorRuntime$new(1)
  tm = TerminatorCombo$new(list(ti, tr))
  pe = TEST_MAKE_PE1()
  pe$start_time = Sys.time() # as in "Tuner$tune()"

  pe$eval_batch(data.table(cp = 0.1))
  expect_false(tm$is_terminated(pe))
  expect_false(ti$is_terminated(pe))
  expect_false(tr$is_terminated(pe))

  Sys.sleep(1)
  expect_true(tm$is_terminated(pe))
  expect_false(ti$is_terminated(pe))
  expect_true(tr$is_terminated(pe))

  tm = TerminatorCombo$new(list(ti, tr), any = FALSE)
  pe = TEST_MAKE_PE1()
  pe$start_time = Sys.time() # as in "Tuner$tune()"

  pe$eval_batch(data.table(cp = 0.1))
  expect_false(tm$is_terminated(pe))
  expect_false(ti$is_terminated(pe))
  expect_false(tr$is_terminated(pe))

  Sys.sleep(1)
  expect_false(tm$is_terminated(pe))
  expect_false(ti$is_terminated(pe))
  expect_true(tr$is_terminated(pe))
})
