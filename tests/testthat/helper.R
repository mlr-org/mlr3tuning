lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_terminator = function(term) {
  expect_r6(term, "Terminator", public = c("eval_before", "eval_after", "remaining"))
  expect_flag(term$terminated)
  expect_string(term$remaining)
}
