context("TerminatorEvaluations")

test_that("API", {
  te = TerminatorEvaluations$new(2)
  expect_terminator(te)
  expect_identical(te$settings$n_evals, 2L)

  pe = TEST_MAKE_PE1()

  pe$eval_batch(data.table(cp = 0.1))
  te$eval_after(pe)
  expect_false(te$terminated)
  # expect_string(te$remaining, pattern = "1 evaluations")

  pe$eval_batch(data.table(cp = 0.2))
  te$eval_after(pe)
  expect_true(te$terminated)
  # expect_string(te$remaining, pattern = "0 evaluations")
})
