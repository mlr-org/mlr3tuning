context("TerminatorPerfReached")

test_that("TerminatorPerfReached", {
  measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
  pe = TEST_MAKE_PE1(measures = measures)
  expect_error({
    terminator = TerminatorPerfReached$new(c(classif.ce = 0.1, false.measure = 0.9), pe)
  })
  expect_error({
    terminator = TerminatorPerfReached$new(c(classif.ce = 0.1, classif.acc = 2), pe)
  }, "<=")
  expect_silent({
    terminator = TerminatorPerfReached$new(c(classif.ce = 0.1, classif.acc = 0.9), pe)
  })
  expect_terminator(terminator)

  gs = TunerGenSA$new(pe, terminator)
  gs$tune()

  aggr = pe$bmr$aggregate(measures)
  thresh = terminator$settings$thresh
  expect_true(any(aggr[["classif.ce"]] <= thresh["classif.ce"] & aggr[["classif.acc"]] >= thresh["classif.acc"]))
})
