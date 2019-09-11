context("TerminatorModelTime")

test_that("API", {
  te = term("model_time", secs = 2)
  inst = TEST_MAKE_INST1()
  inst$eval_batch(data.table(cp = 1))
  inst$start_time = Sys.time() # as in "Tuner$tune()"
  expect_false(te$is_terminated(inst))

  inst$bmr$data$learner[[1]]$state$train_time = c(1, 0) # fake eval time
  expect_false(te$is_terminated(inst))

  inst$bmr$data$learner[[1]]$state$train_time = c(2, 0) # fake eval time
  expect_true(te$is_terminated(inst))
})
