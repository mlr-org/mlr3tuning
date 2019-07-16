context("TunerGenSA")


test_that("TunerGenSA", {
  n_evals = 5
  n_folds = 2

  task = mlr_tasks$get("iris")

  learner = mlr_learners$get("classif.rpart")
  learner$param_set$values$minsplit = 3

  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = n_folds)

  measures = mlr_measures$mget("classif.ce")

  terminator = TerminatorEvaluations$new(n_evals)
  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
  gs = TunerGenSA$new(pe, terminator = terminator, temperature = 200)
  expect_equal(gs$settings$temperature, 200)

  # self = gs
  # private = private(self)
  # x = c(cp = runif(1))

  result = gs$tune()$tune_result()
  bmr = gs$pe$bmr
  expect_r6(gs, "TunerGenSA")
  expect_data_table(bmr$data, nrows = n_folds * n_evals)
  expect_equal(bmr$data[, data.table::uniqueN(hash)], n_evals)
  result = gs$tune()$tune_result()
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 3)
  expect_equal(result$values$minsplit, 3)
})
