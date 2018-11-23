context("TerminatorMultiplexer")

test_that("API", {
  ti = TerminatorIterations$new(2)
  te = TerminatorEvaluations$new(2)
  tm = TerminatorMultiplexer$new(list(ti, te))
  expect_equal(tm$remaining, 2)

  ff = list(bmr = list(data = data.table()))
  tm$update_start(ff)
  tm$update_end(ff)
  expect_false(tm$terminated)
  expect_equal(tm$remaining, 2L)

  expect_identical(tm$terminators[[1]]$state$iters, 0L)
  expect_identical(tm$terminators[[2]]$state$evals, 0L)

  ff$bmr$data = data.table(hash = 1L, dob = 1L)
  tm$update_start(ff)
  tm$update_end(ff)
  expect_identical(tm$terminators[[1]]$state$iters, 1L)
  expect_identical(tm$terminators[[2]]$state$evals, 1L)
  expect_false(tm$terminated)

  ff$bmr$data = data.table(hash = 1L, dob = 2L)
  tm$update_start(ff)
  tm$update_end(ff)
  expect_true(tm$terminated)
  expect_equal(tm$remaining, 0L)

  expect_string(format(tm), fixed = "0 remaining")
})
