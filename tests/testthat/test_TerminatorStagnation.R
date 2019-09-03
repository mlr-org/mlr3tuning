context("TerminatorStagnation")

test_that("TerminatorStagnation", {
  self = ts = TerminatorStagnation$new(iters = 3)
  m = mlr_measures$get("dummy.cp")

  m$minimize = TRUE
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  inst$eval_batch(data.table(cp = seq(from = 0.01, to = 1, length.out = 10)))
  expect_true(ts$is_terminated(inst))

  m$minimize = FALSE
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  inst$eval_batch(data.table(cp = seq(from = 0.01, to = 1, length.out = 10)))
  expect_false(ts$is_terminated(inst))
})
