context("TerminatorMultiplexer")

test_that("API", {
  ff = list(experiments = data.table())
  ti = TerminatorIterations$new(2)
  te = TerminatorEvaluations$new(2)
  tm = TerminatorMultiplexer$new(list(ti, te))
  expect_equal(tm$remaining, 2)

  tm$update_start(ff)
  tm$update_end(ff)
  expect_false(tm$terminated)
  expect_equal(tm$remaining, 1L)

  expect_identical(tm$terminators[[1]]$state$iters, 1L)
  expect_identical(tm$terminators[[2]]$state$evals, 0L)

  ff$experiments = data.table(hash = 1:3)

  tm$update_start(ff)
  tm$update_end(ff)
  expect_true(tm$terminated)
  expect_equal(tm$remaining, 0L)

  expect_string(format(tm), fixed = "0 remaining")
})
