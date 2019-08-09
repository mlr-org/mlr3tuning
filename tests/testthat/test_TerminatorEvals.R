context("TerminatorEvals")

test_that("API", {
  te = TerminatorEvals$new(2)
  expect_terminator(te)
  expect_identical(te$settings$n_evals, 2L)

  pe = TEST_MAKE_PE1()

  pe$eval_batch(data.table(cp = 0.1))
  te$eval_after(pe)
  expect_false(te$is_terminated)

  pe$eval_batch(data.table(cp = 0.2))
  te$eval_after(pe)
  expect_true(te$is_terminated)
})
