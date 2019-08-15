context("TunerGenSA")


test_that("TunerGenSA", {
  test_tuner(TunerGenSA)

  ps = ParamSet$new(params = list(
    ParamLgl$new("save_tasks")
  ))
  term = TerminatorEvals$new(2)
  pe = PerfEval$new("iris", "classif.debug", "holdout", "classif.ce", ps, term)
  tt = TunerGenSA$new()
  expect_error(tt$tune(pe), "support")
})
