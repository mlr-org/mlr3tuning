context("TerminatorMultiplexer")

test_that("API", {
  ti = TerminatorEvaluations$new(2)
  tr = TerminatorRuntime$new(1)
  tm = TerminatorMultiplexer$new(list(ti, tr))
  expect_terminator(tm)

  pe = TEST_MAKE_PE1()

  tm$eval_before(pe)
  pe$eval_batch(data.table(cp = 0.1))
  tm$eval_after(pe)
  expect_false(tm$terminated)
  expect_false(ti$terminated)
  expect_false(tr$terminated)

  tm$eval_before(pe)
  Sys.sleep(1)
  tm$eval_after(pe)
  expect_true(tm$terminated)
  expect_false(ti$terminated)
  expect_true(tr$terminated)

})
