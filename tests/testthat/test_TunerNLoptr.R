test_that("TunerNLoptr", {
  skip_on_os("windows")
  test_tuner("nloptr", x0 = 0.3, algorithm = "NLOPT_LN_BOBYQA", term_evals = 4)
})
