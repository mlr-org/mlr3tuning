context("TerminatorEvaluations")

test_that("API", {
  te = TerminatorEvaluations$new(2)
  expect_terminator(te)
  expect_identical(te$settings$max_evaluations, 2L)
  expect_identical(te$state$evals, 0L)

  bmr = benchmark(expand_grid(
    tasks = mlr_tasks$mget("iris"),
    learners = mlr_learners$mget(c("classif.rpart")),
    resamplings = mlr_resamplings$mget("cv")
  ))
  pe = list(bmr = bmr)

  te$update_start(pe)
  expect_identical(te$state$evals, 1L)
  te$update_end(pe)
  expect_identical(te$state$evals, 1L)
  expect_false(te$terminated)
  expect_output(print(te), "remaining: 1")

  bmr = benchmark(expand_grid(
    tasks = mlr_tasks$mget("iris"),
    learners = mlr_learners$mget(c("classif.featureless", "classif.rpart")),
    resamplings = mlr_resamplings$mget("cv")
  ))
  pe = list(bmr = bmr)

  te$update_start(pe)
  expect_identical(te$state$evals, 2L)
  te$update_end(pe)
  expect_identical(te$state$evals, 2L)
  expect_true(te$terminated)
  expect_output(print(te), "remaining: 0")
})
