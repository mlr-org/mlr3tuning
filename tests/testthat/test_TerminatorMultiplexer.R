context("TerminatorMultiplexer")

test_that("API", {
  ff = list(experiments = data.table())
  ti = TerminatorIterations$new("iter-terminator", 2)
  te = TerminatorEvaluations$new("iter-terminator", 2)
  tm = TerminatorMultiplexer$new("multi-terminator", list(ti, te))
  expect_list(tm$settings, len = 0L)

  tm$update_start(ff)
  tm$update_end(ff)
  expect_false(tm$terminated)

  expect_identical(tm$terminators[[1]]$state$iters, 1L)
  expect_identical(tm$terminators[[2]]$state$evals, 0L)

  ff$experiments = data.table(a = 1:3)

  tm$update_start(ff)
  tm$update_end(ff)
  expect_true(tm$terminated)

  expect_string(tm$message, fixed = "(exhausted)")
})
