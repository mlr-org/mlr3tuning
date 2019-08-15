context("TerminatorPerfReached")

test_that("TerminatorPerfReached", {
  te = TerminatorPerfReached$new(c(dummy.cp = 0.5))
  pe = TEST_MAKE_PE1()
  m = mlr_measures$get("dummy.cp")

  m$minimize = TRUE
  pe = TEST_MAKE_PE1(measures = m)
  pe$eval_batch(data.table(cp = c(0.8, 0.9)))
  expect_false(te$is_terminated(pe))
  pe$eval_batch(data.table(cp = c(0.3)))
  expect_true(te$is_terminated(pe))

  m$minimize = FALSE
  pe = TEST_MAKE_PE1(measures = m)
  pe$eval_batch(data.table(cp = c(0.1, 0.2)))
  expect_false(te$is_terminated(pe))
  pe$eval_batch(data.table(cp = c(0.9)))
  expect_true(te$is_terminated(pe))
})
