context("TerminatorPerfReached")

test_that("TerminatorPerfReached", {
  te = TerminatorPerfReached$new(0.5)
  expect_terminator(te)

  m = mlr_measures$get("dummy.cp")

  m$minimize = TRUE
  pe = TEST_MAKE_PE1(measures = m)
  te$eval_before(pe)
  pe$eval(data.table(cp = c(0.8, 0.9)))
  te$eval_after(pe)
  expect_false(te$terminated)
  te$eval_before(pe)
  pe$eval(data.table(cp = c(0.3)))
  te$eval_after(pe)
  expect_true(te$terminated)

  m$minimize = FALSE
  pe = TEST_MAKE_PE1(measures = m)
  te$eval_before(pe)
  pe$eval(data.table(cp = c(0.1, 0.2)))
  te$eval_after(pe)
  expect_false(te$terminated)
  te$eval_before(pe)
  pe$eval(data.table(cp = c(0.9)))
  te$eval_after(pe)
  expect_true(te$terminated)
})
