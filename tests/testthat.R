if (requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  library("checkmate")
  library("mlr3tuning")
  test_check("mlr3tuning")
}
