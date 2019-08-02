context("TunerGridSearch")


test_that("TunerGridSearch", {
  task = mlr_tasks$get("iris")

  learner = mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)
  resampling = mlr_resamplings$get("cv", param_vals = list(folds = 2L))
  measures = mlr_measures$mget("classif.ce")

  param_set = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))
  pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
  reso = 3L
  term = TerminatorEvaluations$new(10000)
  tt = TunerGridSearch$new(pe, resolution = reso, terminator = term)

  result = tt$tune()$tune_result()
  bmr = tt$pe$bmr
  expect_r6(tt, "TunerGridSearch")
  expect_data_table(bmr$data, nrows = reso * 2)
  expect_equal(bmr$data[, data.table::uniqueN(hash)], reso)
  result = tt$tune()$tune_result()
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 2)
  expect_equal(result$values$minsplit, 3)
})

