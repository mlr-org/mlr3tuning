context("TerminatorEvals")

test_that("API", {
  te = term("evals", n_evals = 2)

  inst = TEST_MAKE_INST1()

  inst$eval_batch(data.table(cp = 0.1))
  expect_false(te$is_terminated(inst))

  inst$eval_batch(data.table(cp = 0.2))
  expect_true(te$is_terminated(inst))
})
