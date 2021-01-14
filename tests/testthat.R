if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(checkmate)
  library(mlr3)
  library(mlr3tuning)
  library(mlr3learners)

  test_check("mlr3tuning")
}
