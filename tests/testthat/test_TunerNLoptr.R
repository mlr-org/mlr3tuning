test_that("TunerNLoptr", {
  skip_on_os("windows")
  test_tuner("nloptr", algorithm = "NLOPT_LN_BOBYQA", term_evals = 4)
})
