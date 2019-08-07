context("TerminatorEvaluations")

test_that("API", {
  te = TerminatorEvaluations$new(2)
  expect_terminator(te)
  expect_identical(te$settings$n_evals, 2L)

  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1)
  ))
  measure = mlr_measures$mget("classif.ce")
  rs = mlr_resamplings$get("cv", param_vals = list(folds = 2))
  pe = PerfEval$new(task, lrn, rs, measure, ps)

  pe$eval(data.table(cp = 0.1))
  te$eval_after(pe)
  expect_false(te$terminated)
  # expect_string(te$remaining, pattern = "1 evaluations")

  pe$eval(data.table(cp = 0.2))
  te$eval_after(pe)
  expect_true(te$terminated)
  # expect_string(te$remaining, pattern = "0 evaluations")
})
