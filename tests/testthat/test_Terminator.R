context("Terminator")

test_that("API", {
  t = Terminator$new(list())
  
  expect_error(t$update_start())
  expect_error(t$update_end())
})
