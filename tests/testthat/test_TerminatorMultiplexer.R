context("TerminatorMultiplexer")

test_that("API", {
  ti = TerminatorEvaluations$new(2)
  tr = TerminatorRuntime$new(1)
  tm = TerminatorMultiplexer$new(list(ti, tr))
  expect_terminator(tm)

  expect_equal(tm$settings$max_evaluations, ti$settings$max_evaluations)
  expect_equal(tm$settings$runtime, tr$settings$runtime)

  bmr = benchmark(expand_grid(
    tasks = mlr_tasks$mget("iris"),
    learners = mlr_learners$mget(c("classif.rpart")),
    resamplings = mlr_resamplings$mget("cv")
  ))
  pe = list(bmr = bmr)

  tm$update_start(pe)
  Sys.sleep(0.1)
  tm$update_end(pe)
  expect_false(tm$terminated)
  expect_false(ti$terminated)
  expect_false(tr$terminated)

  tm$update_start(pe)
  Sys.sleep(1)
  tm$update_end(pe)
  expect_true(tm$terminated)
  expect_false(ti$terminated)
  expect_true(tr$terminated)

  expect_equal(tm$terminators[[1]]$state$evals, 1L)
  expect_equal(tm$terminators[[2]]$settings$runtime, 1L)

  expect_output(print(tm), "remaining: 1")
  expect_output(print(tm), "-0")
})
