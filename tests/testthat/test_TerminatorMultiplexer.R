context("TerminatorMultiplexer")

test_that("API", {
  ti = TerminatorEvaluations$new(2)
  tr = TerminatorRuntime$new(1)
  tm = TerminatorMultiplexer$new(list(ti, tr))
  expect_terminator(tm)

  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1)
  ))
  measure = mlr_measures$mget("classif.ce")
  rs = mlr_resamplings$get("cv", param_vals = list(folds = 2))
  pe = PerfEval$new(task, lrn, rs, measure, ps)

  tm$eval_before(pe)
  pe$eval(data.table(cp = 0.1))
  tm$eval_after(pe)
  expect_false(tm$terminated)
  expect_false(ti$terminated)
  expect_false(tr$terminated)

  tm$eval_before(pe)
  Sys.sleep(1)
  tm$eval_after(pe)
  expect_true(tm$terminated)
  expect_false(ti$terminated)
  expect_true(tr$terminated)

  # expect_string(tm$remaining, pattern = "1 evaluations")
  expect_string(tm$remaining, pattern = "-0")
})
