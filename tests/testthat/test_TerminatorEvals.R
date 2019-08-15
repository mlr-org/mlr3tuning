context("TerminatorEvals")

test_that("API", {
  te = TerminatorEvals$new(2)

  pe = TEST_MAKE_PE1()

  pe$eval_batch(data.table(cp = 0.1))
  expect_false(te$is_terminated(pe))

  pe$eval_batch(data.table(cp = 0.2))
  expect_true(te$is_terminated(pe))
})
