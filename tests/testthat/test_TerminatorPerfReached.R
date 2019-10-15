context("TerminatorPerfReached")

test_that("TerminatorPerfReached", {
  te = term("perf_reached", level = 0.5)
  inst = TEST_MAKE_INST1()
  m = mlr_measures$get("dummy.cp.classif")

  m$minimize = TRUE
  inst = TEST_MAKE_INST1(measures = m)
  inst$eval_batch(data.table(cp = c(0.8, 0.9)))
  expect_false(te$is_terminated(inst))
  inst$eval_batch(data.table(cp = c(0.3)))
  expect_true(te$is_terminated(inst))

  m$minimize = FALSE
  inst = TEST_MAKE_INST1(measures = m)
  inst$eval_batch(data.table(cp = c(0.1, 0.2)))
  expect_false(te$is_terminated(inst))
  inst$eval_batch(data.table(cp = c(0.9)))
  expect_true(te$is_terminated(inst))
})
