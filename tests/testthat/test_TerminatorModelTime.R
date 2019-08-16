context("TerminatorModelTime")

test_that("API", {
  te = TerminatorModelTime$new(2)
  pe = TEST_MAKE_PE1()
  pe$eval_batch(data.table(cp = 1))
  pe$start_time = Sys.time() # as in "Tuner$tune()"
  expect_false(te$is_terminated(pe))

  pe$bmr$data$learner[[1]]$state$train_time = c(1, 0) # fake eval time
  expect_false(te$is_terminated(pe))

  pe$bmr$data$learner[[1]]$state$train_time = c(2, 0) # fake eval time
  expect_true(te$is_terminated(pe))
})
