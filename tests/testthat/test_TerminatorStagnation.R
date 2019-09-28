context("TerminatorStagnation")

test_that("TerminatorStagnation", {
  te = term("stagnation", iters = 3)
  m = mlr_measures$get("dummy.cp.classif")

  m$minimize = TRUE
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  inst$eval_batch(data.table(cp = seq(from = 0.01, to = 1, length.out = 10)))
  expect_true(te$is_terminated(inst))

  m$minimize = FALSE
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  inst$eval_batch(data.table(cp = seq(from = 0.01, to = 1, length.out = 10)))
  expect_false(te$is_terminated(inst))

  ####
  m = msr("dummy.cp.classif", minimize = TRUE)

  # never stagnate
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  design = data.table(cp = c(seq(from = 0.5, to = 0.0, length.out = 20)))
  te = term("stagnation", iters = 3)
  for (i in seq_row(design)) {
    lgr::without_logging(inst$eval_batch(design[i]))
    if (te$is_terminated(inst))
      break
  }
  tab = inst$archive()
  expect_data_table(tab, nrows = 20)

  # last improvement in step 10

  # make sure cp values define different exps, but the scores are the same (so we stagnate)
  m = msr("dummy.cp.classif", minimize = TRUE, fun = function(x) round(x, digits = 2))
  design = data.table(cp = c(seq(from = 0.5, to = 0.3, length.out = 10),
    seq(from = 0.2999, to = 0.299, length.out = 10)))
  inst = TEST_MAKE_INST1(measures = m, term_evals = 100)
  for (i in seq_row(design)) {
    lgr::without_logging(inst$eval_batch(design[i]))
    if (te$is_terminated(inst))
      break
  }
  tab = inst$archive()
  expect_data_table(tab, nrows = 13)
})
